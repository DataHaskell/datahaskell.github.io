---
layout: post
title:  "Writing Parquet Files Using Haskell"
date:   2026-09-16 11:41:21 +0530
categories: blog
author: Raghav Sharma
---


We implemented a parquet writer in [DataHaskell/Dataframe](https://github.com/DataHaskell/dataframe). Using it is simple; you need only pass your dataframe into the `writeParquet` function which writes a parquet file with sane defaults for row group and page sizes. An example:

```haskell
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import DataFrame (as, (|>))

main = do
    sales <- D.readParquet "sales_data.parquet"
    
    sales
        |> D.groupBy ["product"]
        |> D.aggregate [ F.sum (F.col @Int "amount") `as` "total"
                       , F.count (F.col @Int "amount") `as` "orders"
                       ]
        |> D.writeParquet "total_orders.parquet"
```

If you need more fine-grained control over the parquet file you'll want to use `writeParquetWithOptions`. Read on to see what those options are and how they affect the final file.

## Parquet For Haskell

For Haskell to interoperate with the data ecosystem, it must be able to understand the standard formats in use by that ecosystem. For a long time our options for serializing data[^0] in Haskell came down to CSV, JSON, or to simply dump it into a ByteString. While CSV and JSON have their place, they come with considerable disadvantages as the volume of data expands. They are slow and cumbersome to read, write, and query, and any custom homegrown formats tend to lack interoperability with standard data science tools.

The parquet format trades simplicity for efficient storage and querying of the data stored therein. We want our data to have high compression ratios and we want to minimize reading data irrelevant to our specific query. The ability to read and write parquet files for long term storage, sending over the network, or for interop with other programs, especially given the universality of Parquet in the data science ecosystem, is a rather useful tool that we should like to have in a Dataframe library.

## Parqué?

Feel free to skip this section if you already know the structure of a parquet file. 

Parquet files are a series of row groups followed by metadata at the end of the file with information that allows readers to locate relevant column chunks and pages. It also contains useful information like statistics and bloom filters so that, for example, a reader / query planner can decide whether or not to read a specific row group.

Each row group is a collection of column chunks, each of which contain the same number of rows. Each column chunk is a series of data pages. Since each column chunk is a series of pages, and each row group is a series of column chunks, the final file simply looks like a series of pages from each column one after the other. We're able to make sense of it all by using the metadata to identify the offset and size of each row group and column chunk.

A data page is where we actually store all of our data. It consists of first the page metadata, describing its encoding, the number of values, the statistics, among other things. There are actually [two](https://github.com/apache/parquet-format/blob/master/src/main/thrift/parquet.thrift#L699) [versions](https://github.com/apache/parquet-format/blob/master/src/main/thrift/parquet.thrift#L752) of the data page with subtle differences.

Next we have definition levels and repetition levels. They're an inexpensive encoding of nullability and nested structure. A detailed description of definition levels and repetition levels is out of scope for this article; refer to the [Dremel paper](https://static.googleusercontent.com/media/research.google.com/en//pubs/archive/36632.pdf) for that. For our purposes we currently only support writing definition levels up to one to denote nullable values. 

Finally we have our actual [encoded](https://github.com/apache/parquet-format/blob/master/Encodings.md) and [compressed](https://github.com/apache/parquet-format/blob/master/Compression.md) data (depending on which data page we're using we compress either just the data or both the definition/repetition levels and the data). The encoding is determined per page, while compression is determined per column chunk. 

## The Implementation

What follows is a technical discussion of the design of the Parquet writer and the tradeoffs we made.
### Write Options

It follows from the description of the parquet format above that a writer needs to expose the appropriate set of fiddle factors to allow the user to tune the writer to produce a parquet file that is efficient for the specific data it is being applied to. That is, we want a parquet writer that compresses well with appropriately sized row groups and pages, so that a reader can effectively apply parallelism, projection pushdown (read only relevant column chunks), predicate pushdown (read only relevant row groups/ prune irrelevant pages), IO pushdown (read only relevant files, assuming the data is chunked into multiple files), etc[^1].

```haskell
data ParquetWriteOptions = ParquetWriteOptions
    { pageSize :: !Int
    , rowGroupSize :: !Int
    , batchRows :: !Int
    , subBatchRows :: !Int
    , compressionCodec :: !CompressionCodec
    , strategy :: !WriterStrategy
    , maxRowsPerFile :: !(Maybe Int)
    }

```

Both `pageSize` and `rowGroupSize` are the target size in bytes of each page and each row group. But each column chunk in a row group must contain the same number of rows, and depending on the specific data being encoded, the encoding, and the compression algorithm we're using, each column chunk will hold a different number of rows before reaching the target size. The same holds true for each page. So how do we ensure our page size target, our row group size target, and have the same number of rows in each column chunk?

We must consider both the target `pageSize` and `rowGroupSize` to be best effort; they could be somewhat above or below the target. We run our columns through batches of size `batchRows` and check the state of the row group after each batch. So each row group contains an integer multiple of `batchRows` rows; pages also will contain an integer multiple of `subBatchRows` rows (with the exception of the final page and the final column chunk). Sub-batching at the page level allows us to reduce the amount of IORef book-keeping we have to do after each write, granting us significant speedups.[^2]

### Memory

Due to the constraints described in the previous section, it's difficult to know how big to make our buffers ahead of time. This is even more true of column chunk buffers -- each column will fit a very different number of rows / pages in the same amount of data, and we may expect that some column chunk buffers will be significantly bigger than others and dominate the space per row group. 

So we must be able to grow our memory. One of the more convenient ways of handling raw memory without resorting to the FFI is `MutableByteArray`. 

```haskell
data MemoryBuffer = MemoryBuffer
    { arrayRef :: !(IORef (MutableByteArray RealWorld))
    , positionRef :: !(IORef Int)
    }
```

For our implementation we use pinned `ByteArray`s, as we would like to convert it into a `Ptr Word8` when it's time to flush into either another buffer, for example when flushing a page buffer into a column chunk buffer, or into a file, as we would when flushing a row group to file.

Using pinned `ByteArray`s results in a slight complication when trying to grow the memory buffer. We must not use the `grow` function provided by `Data.Primitive`, instead we must allocate a new pinned ByteArray and allow the old one to be GCed. One might be worried about heap fragmentation because a single pinned object in a 4KB GHC block can keep the whole block alive but  we expect that our buffers will tend to be much larger than 4KB. Furthermore grows ought to be rare especially after the first few Pages and the first RowGroup.

```haskell
ensureCapacity :: MemoryBuffer -> Int -> IO (MutableByteArray RealWorld)
ensureCapacity buffer needed = do
    array <- readIORef buffer.arrayRef
    maxSize <- getSizeofMutableByteArray array
    if needed <= maxSize
        then pure array
        else do
            position <- readIORef buffer.positionRef
            grown <- newPinnedByteArray (needed + (needed `div` 2))
            copyMutableByteArray grown 0 array 0 position
            writeIORef buffer.arrayRef grown
            pure grown
{-# INLINE ensureCapacity #-}

```
 
We also write helper functions for writing `Word8`s, `Word32`s, `Word64`s, `Int32`s, `Int64`s, `Integer`s, `Float`s, `Double`s, and `ByteString`s to a buffer. Finally, we have a `flushBufferToBuffer :: MemoryBuffer -> MemoryBuffer -> IO ()` and a `flushBufferToFile :: WritableBinaryHandle -> MemoryBuffer -> IO ()`[^3].

### The Core Loop

Here we provide a high level sketch of the main loop of the Parquet Writer. There is considerable complexity we omit in favor of explaining the design as simply as possible. For the full implementation refer to [Writer.hs](https://github.com/DataHaskell/dataframe/blob/main/dataframe-parquet/src/DataFrame/IO/Parquet/Writer.hs).

At a high level we can think of the Parquet writer as an effectful fold over the dataframe[^4] (Also known as `reduce` in some languages or, more rare, `accumulate`). A fold can be thought of as a loop, that is, we iterate over the rows in our dataframe. Folds are usually pure, but 'effectful' means that each iteration produces some kind of side-effect, in our case writing to and from memory and to disk. Finally the fold produces a final singular result from the iteration, and that is the file metadata that we must append to the end of the file.

First we must define the state threaded through the writer to accomplish this:

```haskell
data ParquetWriterState = ParquetWriterState
    { outputFileHandle :: !WritableBinaryHandle -- newtype over a handle specifically for us to write to
    , columnChunks :: !(VB.Vector ColumnChunkState) -- effectively our row group buffer
    , currentFileOffsetRef :: !(IORef Int64)
    , scratchBuffer :: !MemoryBuffer
    , rowGroupMetadataRef :: !(IORef [RowGroup])
    , rowNumberRef :: !(IORef Int)
    }

data ColumnChunkState = ColumnChunkState
    { columnName :: !T.Text
    , nullable :: !Bool
    , schema :: !SchemaElement
    , encoder :: !Encoder
    , buffer :: !MemoryBuffer
    , uncompressedBufferSize :: !(IORef Int64)
    , pageState :: !PageState
    }

data PageState = PageState
    { pageBuffer :: !MemoryBuffer -- Each column has its own pageBuffer
    , definitionLevels :: !DefLevels
    , currentRowCount :: !(IORef Int)
    }
    
-- In Encoder.hs

data Encoder = Encoder
    { encType :: !ThriftType
    , convertedType :: !(Maybe ConvertedType)
    , logicalType :: !(Maybe LogicalType)
    , encodeValue :: !(MemoryBuffer -> Int -> Int -> IO (Int, Bool))
    , finishValues :: !(MemoryBuffer -> Int -> IO Int) -- in case cleanup or post processing is required
    }
```
The buffers for each ColumnChunk are flushed into the final file while the individual page buffers are flushed into ColumnChunk buffers (or more accurately are used to assemble the data that is eventually flushed into the ColumnChunk buffer as we need to flush the data along with the page metadata and definition levels).

If we dispense with the ceremony of setting up the initial `writerState` and other variables, our core loop is quite simple

```haskell
loop :: Int -> IO ()
loop rowNum
    | rowNum >= endRow = pure ()
    | otherwise = do
        let batchEnd = rowNum + min interval (endRow - rowNum)
        writeBatch rowNum batchEnd
        size <- bufferedSize writerState.columnChunks
        when
            (size >= options.rowGroupSize)
            (flushRowGroup options writerState) -- Write to file
        loop batchEnd
```

then `writeBatch`:

```haskell
writeBatch :: Int -> Int -> IO ()
writeBatch rowNum batchEnd
    | rowNum >= batchEnd = pure ()
    | otherwise = do
        let count = min options.subBatchRows (batchEnd - rowNum)
        forM_ writerState.columnChunks (writeRows options scratchBuffer rowNum count)
        modifyIORef writerState.rowNumberRef (+ count)
        writeBatch (rowNum + count) batchEnd
        
```

The `scratchBuffer` is a re-usable buffer that we'll later use to assemble the page as the individual page buffers hold only the values, but our definition levels live in a different buffer and the page metadata doesn't exist yet. It is the `scratchBuffer` that is eventually flushed to a ColumnChunk buffer.

The internal workings of `writeRows` are somewhat similar to the `loop` we saw earlier.

```haskell
rowWriterLoop !options !columnChunkState !end !size !position !row
    | row >= end = writeIORef columnChunkState.pageState.pageBuffer.positionRef position
    | position + options.pageSize > size = do
        let page = columnChunkState.pageState
        writeIORef page.pageBuffer.positionRef position
        arr' <- ensureCapacity
                    page.pageBuffer
                    (position + max
                                    options.pageSize
                                    ((end - row) * 64)
                    )
        size' <- getSizeOfMutableByteArray arr'
        rowWriterLoop options columnChunkState end size' position row
    | otherwise = do
        let page = columnChunkState.pageState
            encode = columnChunkState.encoder.encodeValue
        (position', notNull) <- encode page.pageBuffer position row
        when columnChunkState.nullable $
            pushDef page.definitionLevels (if notNull then 1 else 0)
        rowWriterLoop options columnChunkState end size position' (row + 1)
```

Note that we only handle definition levels up to 1 for our writer and we don't support definition levels greater than 1 and repetition levels greater than 0 as, currently, we specifically only want to support dataframes which have flat schemas.

When a page is filled up we can flush it to the parent `columnChunkState.buffer`.

```haskell
pageResidency <- readIORef page.pageBuffer.positionRef
defResidency <- readIORef page.definitionLevels.dlBuf.positionRef
when
    (pageResidency + defResidency >= options.pageSize)
    (flushPage options scratchBuffer columnChunkState) 
```

## Future Work

Currently the Parquet Writer supports only
- Snappy and Uncompressed compression
- Plain Encoding
- Only definition levels up to 1

The next step would first be to support the full gamut of compressions and encodings offered by Parquet. The current Writer is also completely single threaded, so we may explore adding concurrency and/or parallelism in limited ways -- so long as there's a performance justification for doing so.

Currently optional metadata fields like statistics for columns/pages and bloom filters are not recorded. As we mentioned earlier, these are very useful for parquet readers to optimize querying the data in a parquet file.

Finally, currently all of our buffers live in memory while they wait to be flushed to disk. In memory constrained systems writing Parquet files that require particularly large pages/rowgroups, we may end up running out of memory (an entire row group must be held in memory in its entirety). So we plan to implement a two pass strategy where we use much smaller in memory buffers and write to on-disk temporary files instead of keeping data in memory. 

---

[^0]: In this article when we speak of data we usually mean copious amounts of columnar data that one is wont to put in a dataframe.

[^1]: The above optimizations also require the implementation of multiple encodings, statistics, bloom filters, and so on. The current implementation focuses on getting the basic writer up and running and the optional parts of parquet files are planned as future work. 

[^2]: There is a possibility, if there are columns whose individual entries are disproportionately large, that our sub-batching/batching overshoots the target `pageSize` and/or `rowGroupSize` by orders of magnitude. A mechanism to mitigate this effect may need to be implemented if users start running into this issue.

[^3]: The `flushBufferToFile` implementation lead me down a somewhat interesting rabbit hole. By my meagre understanding: it turns out that by default when writing to file we're actually writing to the OS page cache, which marks a page as dirty, and if the ratio of dirty pages exceeds a certain value, the OS throttles the process producing dirty pages while writeback catches up, which, as we're writing row groups synchronously, forces us to wait while the Kernel sorts things out (`hPutBuf` uses `write(2)` internally, if I'm not very much mistaken). 

    So it behooves us to write data in large enough chunks that we write more data with fewer syscalls (though very large chunks will have diminishing returns). After using `dd` to run some tests on my computer, I settled on 256 KiB chunks as a reasonable default for our Parquet Writer.

    ```haskell
    -- for use in a single threaded writer
    flushBufferToFile :: WritableBinaryHandle -> MemoryBuffer -> IO ()
    flushBufferToFile (WritableBinaryHandle h) buffer = do
        array <- readIORef buffer.arrayRef
        position <- readIORef buffer.positionRef
        withMutableByteArrayContents array $ \ptr -> do
            let chunkSize = 262144
                go offset
                    | offset >= position = pure ()
                    | otherwise = do
                        let n = min chunkSize (position - offset)
                        -- the handle is not block buffered
                        hPutBuf h (ptr `plusPtr` offset) n
                        go (offset + n)
            go 0
        writeIORef buffer.positionRef 0
    ```

    It's unlikely we would need it, but should we ever desire writing to disk directly without the mediation of the kernel, in spite of the extra effort to make it cross platform, `O_DIRECT` beckons. The rabbit hole goes deeper when we consider multithreading and avoiding losing data due to a crash.

[^4]: We say 'Effectful fold over the dataframe', but if you look at the code we don't actually use a `foldM`. Mostly the code is recursive and a `forM_` is used. This is the local minimum we landed on to make it convenient to do the batching and sub-batching required by the writer. However, the statement that it is an effectful fold is still accurate as `forM_` is itself a special kind of fold and folds abstract over recursion (See Graham Hutton's paper "[A Tutorial on the Universality and Expressiveness of Fold](https://people.cs.nott.ac.uk/pszgmh/fold.pdf)")
