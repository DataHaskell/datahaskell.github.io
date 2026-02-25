---
layout: post
title:  "beam-duckdb: a Beam backend for typed SQL over DuckDB"
date:   2026-02-25 17:27:12 +0100
categories: blog
---

The dataHaskell organization is spearheading the development of a slew of tools for data science. However, sometimes, all you need is SQL.

The Haskell ecosystem is home to many database libraries adding a layer of type safety over SQL queries. One such pioneering library is [`beam`](https://github.com/haskell-beam/beam), which provides an embedded domain-specific language adding strong guarantees over SQL queries. One key design principle of `beam` is that it is _database-agnostic_; `beam` adapts its functionality to the specific database, which has traditionally been PostgreSQL, SQLite, or MySQL.

This blog post is about the addition of a new supported database, bringing the power of `beam` over to [DuckDB](https://duckdb.org/).

### Why DuckDB

DuckDB is an in-process database, much like SQlite, but with a focus on analytics instead of transaction processing. Specifically, DuckDB focuses on online analytical processing (OLAP) workloads.

If you want to drive DuckDB from a Haskell program , you can use [`duckdb-simple`](https://hackage.haskell.org/package/duckdb-simple):

```haskell
import Database.DuckDB.Simple

main :: IO
main = do
  Only (maxScore :: Double) <- withConnection ":memory:" $ \conn ->
    query_
      conn
      "SELECT MAX(score) \
      \FROM read_parquet('scores.parquet');"

  putStrLn "Max exam score: "
  print maxScore
```

This interface is very similar to the experience that you would get using SQLite via `sqlite-simple`, but here you get access to DuckDB's high performance. However, there's no checking the query at compile-time; for example, we'll get a runtime error if the result of the query is not, in fact, a `Double`.

Note here that we're loading data from a Parquet file using `read_parquet`. DuckDB supports loading data from various data sources, including file formats such as Parquet and CSV, and data lake formats such as Apache Iceberg and Delta Lake.

In summary, DuckDB is a great choice of database for data science workloads because:
* Its query engine is optimized for analytical workloads;
* It is SQL-compliant;
* It provides SQL extensions to interact with various data sources important to data science.

### Adding structure with beam

Once you have determined a good pipeline for your workload, you might want to add some structure to prevent mistakes. This is where `beam` comes in.

`beam`, at its core, is a database library that generates SQL statements from a type-safe domain-specific language embedded in Haskell. Since DuckDB supports the various standards that make up modern SQL, it was possible to write a backend for `beam` to be used to generate statements for DuckDB.

With `beam`, we first describe the schema for our data in Haskell:

```haskell
-- Represents a table of exam scores.
data ExamT f = Exam
  { _examId :: Columnar f Int32,
    _examName :: Columnar f Text,
    _examScore :: Columnar f Double,
    _examDate :: Columnar f Day
  }

type Exam = ExamT Identity
```

We can declare the database as having a source from a Parquet file like so:

```haskell
data ScoresDB f = ScoresDB
  { _scores :: f (DataSourceEntity ExamT),
  }
  deriving (Generic, Database DuckDB)

scoresDb :: DatabaseSettings DuckDB ScoresDB
scoresDb =
  defaultDbSettings
    `withDbModification` (dbModification @_ @DuckDB)
      { _scores =
          dataSource (parquet (NonEmpty.singleton "scores.parquet"))
            <> modifyDataSourceFields
              tableModification
                { _examId = "id",
                  _examName = "name",
                  _examScore = "score",
                  _examDate = "exam_date"
                }
      }
```

Finally, we can query the data source:

```haskell
main = do
  Just maxPrice <- withConnection ":memory:"
    $ \conn -> runBeamDuckDB conn
      $ runSelectReturningOne
        $ select
          $ aggregate_
              (max_ . _examScore)
              (allFromDataSource_ (_scores scoresDb))

  putStrLn "Max exam score: "
  print maxScore
```

[Beam makes it possible to construct much more powerful queries](https://haskell-beam.github.io/beam/). For example, what's the whole row associated with the highest score?

```haskell
main = do
  Just rowWithMaxScore <- withConnection ":memory:"
    $ \conn -> runBeamDuckDB conn
      $ runSelectReturningOne
        $ select
          $ do
            -- subquery : SELECT MAX(score) FROM read_parquet(...)
            let findMaxScore =
                  aggregate_
                    (max_ . _examScore)
                    (allFromDataSource_ (_dbExams testDb))

            -- top-level query: SELECT * FROM read_parquet(...) where score=subquery
            e <- allFromDataSource_ (_dbExams testDb)
            guard_ (just_ (_examScore e) ==. subquery_ findMaxScore)
            pure e

  putStrLn $  "The max exam score of "
           <> show (_examScore rowWithMaxScore)
           <> " was scored by "
           <> show (_examName rowWithMaxScore)
           <> " on "
           <> show (_examDate rowWithMaxScore)
```

and voilà!

With the added structure of the schema (represented by the type `ExamT`), we get:
* a very smart compiler checking the validity of our queries at compile-time;
* composable queries that can be combined in a way that raw SQL cannot.

------------------------------

The `beam-duckdb` backend is very new. Do you want to use this but don't know where to start? Do you wish it supported features that are not currently implemented? Do not hesitate to [raise an issue](https://github.com/haskell-beam/beam/issues/new)! 
