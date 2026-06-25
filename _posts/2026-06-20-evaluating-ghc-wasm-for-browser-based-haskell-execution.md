---
layout: post
title:  "What I Learned Studying GHC WASM for Browser-Based Haskell"
date:   2026-06-20 12:56:00 +0530
categories: blog
---

# What I Learned Studying GHC WASM for Browser-Based Haskell

*Pulling apart the architecture of GHC in Browser to prepare for a WebAssembly-backed Jupyter kernel*


## Why This Matters

My Google Summer of Code 2026 project with Haskell.org is to bring a GHC/WebAssembly backend to **xeus-haskell** — a Jupyter kernel for Haskell that currently runs on MicroHs. The goal: let people write real GHC Haskell inside JupyterLite, with the full language and a broad slice of Hackage, without needing a server.

Before building that, I needed to understand how someone has already made GHC run entirely in a browser. **GHC in Browser** is exactly that project, and studying it gave me the clearest picture of what's involved.

Honestly, going in, I expected the main challenge to be getting the GHC codebase to compile to WebAssembly at all. What I found was different — and more interesting — than what I anticipated. Here's what I came away with:

- **GHC can genuinely run client-side.** The compiler, the RTS, the package database — all of it can live inside a browser tab once you compile GHC to WebAssembly.
- **The hard problems aren't compilation — they're environment.** Before looking at the project, I assumed getting GHC itself into WebAssembly would be the hardest part. What surprised me was that many of the difficult problems appear after compilation: providing filesystem access, I/O, and enough operating-system functionality for the compiler to feel at home.
- **WASI is the bridge that makes it practical.** The WebAssembly System Interface gives GHC the POSIX-like surface it expects without breaking the browser's security sandbox. I hadn't appreciated how much of the heavy lifting WASI does until I traced through the actual call paths.
- **The architecture appears to map well onto xeus-haskell.** At a high level, the same layered approach — Wasm execution, virtual filesystem, JavaScript glue — should apply to building a notebook kernel, although I expect additional challenges to surface once notebook execution semantics enter the picture.

The rest of this post walks through the architecture in detail. If you're interested in GHC WASM, browser-native dev tools, or just want to understand how a 30-year-old compiler ends up running in a browser tab, read on.

## The Case for Browser-Native Haskell

Today, using Haskell interactively usually means one of two things: install GHC locally, or connect to a server that runs it for you. Both create friction. A student in a classroom has to fight with toolchain setup before writing a single line of code. A tutorial author has to maintain infrastructure or hope readers will figure out the install themselves.

Running Haskell directly in the browser changes that equation:

* A student can open a URL and start coding immediately — no install, no setup.
* Educators can embed live Haskell environments in course materials.
* Browser-based notebooks like JupyterLite can execute Haskell without a remote kernel.
* Once the initial download finishes, everything works offline.
* The browser's security sandbox provides isolation for free.

These are the reasons I care about GHC WASM specifically — not as a curiosity, but as the foundation for making xeus-haskell work without server infrastructure. When I first started reading about WebAssembly in the context of compilers, I wasn't sure how far the technology had actually come. Seeing GHC in Browser work end-to-end was the moment it clicked for me that this was a real path forward, not just an interesting proof of concept.

## How GHC in Browser Works

With that context in mind, let's look at how the existing GHC in Browser project actually pulls this off. From a user's perspective, the workflow is dead simple:

1. Open the web app.
2. Write some Haskell.
3. Hit Run.
4. See the result.

Behind that simplicity, several components cooperate to create an environment where GHC can do its thing.

![Understanding GHC in Browser](/images/1.Understanding%20Ghc%20browser%20diagram.png)

The browser hosts both the UI and the compiler runtime. A virtual filesystem, a WebAssembly runtime, and JavaScript glue code provide the environment that GHC expects during compilation and execution.

### Traditional vs Browser-Native Execution

Most online programming environments follow a familiar pattern: the browser is a thin client, and compilation happens on a remote server.

![Traditional vs native execution](/images/2.traditional%20vs%20native%20diag.png)

GHC in Browser flips this around. After the initial load, all compilation and execution happens locally in the user's browser session.

![Browser-native execution](/images/3.native%20diag.png)

This isn't just a deployment detail — it changes the fundamental assumptions. There's no round-trip latency for compilation, no server to maintain, and no scaling concerns when more users show up. I'll admit that when I first heard "GHC runs in the browser," I half-expected there to be a server hiding behind the scenes. There isn't. Once the Wasm module loads, the browser is doing all the work.

### The Questions I Focused On

While studying the project, I kept coming back to four questions:

1. How is GHC compiled to WebAssembly in the first place?
2. How does the browser provide the environment GHC expects (files, I/O, etc.)?
3. How are compiled artifacts loaded and executed?
4. How does the system stay responsive while GHC is compiling?

The rest of this post works through the components that answer each of these.

## Architecture: Four Layers

To answer the questions above, it helps to view the system as a set of cooperating layers.

![Architecture overview](/images/4.architecure%20diag.png)

| Layer | What It Does |
| :---- | :---- |
| **User Interface** | Code editing and result display |
| **JavaScript Runtime** | Module loading and execution coordination |
| **GHC WebAssembly Runtime** | The compiler and interpreter, compiled to Wasm |
| **Browser Platform** | WebAssembly execution engine and browser APIs |

### The Role of WebAssembly

At the core of the whole thing is WebAssembly. Instead of running GHC as a native binary, the compiler is built targeting the Wasm instruction set and executed inside the browser's Wasm runtime.

This is what makes "GHC in the browser" possible at all. Once the Wasm module has been downloaded and instantiated, you have what is effectively a full Haskell toolchain running on the client. No server involved. Seeing this for the first time was a bit surreal — the same compiler I'd been installing through GHCup was now sitting inside a browser tab.

![WebAssembly execution model](/images/5.webassembly%20diag.png)

WebAssembly handles the heavy lifting of execution, while JavaScript acts as the bridge to browser-specific functionality — things like DOM interaction, async scheduling, and network access.

### Bridging JavaScript and GHC

One thing that wasn't immediately obvious to me is how much work the JavaScript layer does. I initially assumed JavaScript was just there to load the page and kick off the Wasm module. In reality, GHC compiled to Wasm is just a binary — it doesn't know how to talk to the browser. The JavaScript runtime handles:

- **Loading the Wasm module** and instantiating it with the right imports.
- **Providing callback functions** that GHC calls when it needs to interact with the outside world (printing output, reading input, accessing files).
- **Managing async coordination** so that long-running compilations don't freeze the browser tab.

This JavaScript glue is the connective tissue between "GHC as a Wasm binary" and "GHC as a usable browser application." It's also the layer where I expect most of the xeus-haskell integration work will happen — the Jupyter protocol messages will need to flow through this same boundary.

### The Role of WASI

GHC assumes it's running on something like a Unix system. It wants to open files, read directories, write to stdout, and access environment variables. Browsers don't offer any of that.

The WebAssembly System Interface (WASI) fills this gap. It defines a standardised set of APIs that Wasm programs can call to interact with their host environment in a controlled way.

![WASI working diagram](/images/6.wasi%20working.png)

In practice, WASI lets GHC call functions like `fd_read` and `fd_write` as if it were making normal system calls. The browser-side WASI implementation translates those calls into operations on an in-memory virtual filesystem or JavaScript-managed I/O streams.

This turned out to be one of the cleanest parts of the architecture, and the part that shifted my thinking the most. GHC doesn't need any browser-specific modifications — it just talks to WASI, and the host environment figures out the rest. Before studying this, I had vaguely assumed that running GHC in a browser would require patching the compiler itself. The fact that WASI lets it work largely unmodified was a genuine surprise.

### Why a Virtual Filesystem Is Needed

A traditional GHC installation relies heavily on the filesystem. Base libraries, package databases, interface files, temporary compilation artifacts — GHC reads and writes files constantly during compilation.

Browsers don't have a filesystem. So GHC in Browser creates one in memory, pre-populated with everything the compiler needs:

* GHC's runtime resources and configuration
* The standard Haskell libraries (base, containers, text, etc.)
* Package registration files
* A writable area for temporary files generated during compilation

From the compiler's point of view, it's just reading and writing files as usual. It has no idea those "files" are backed by JavaScript arrays in memory rather than an actual disk.

This is the kind of detail that's easy to overlook until you try to build something similar. When I first thought about what xeus-haskell would need, I was focused on the compiler and the Jupyter protocol. The virtual filesystem didn't even occur to me as a major piece of work. Now I think it might be one of the more involved parts — the notebook kernel will need the same kind of pre-populated environment, likely with additional packages depending on what users want to import, and managing that package set at runtime could get complicated.

## Execution Flow

With the environment set up, here's what actually happens when you press Run.

### From Source Code to Result

The user's code goes through the full GHC pipeline: parsing, type checking, desugaring, simplification, code generation, and finally execution of the compiled output.

![Execution flow](/images/7.execution%20flow.png)

From the user's perspective this looks like a simple request-response cycle, but internally it's the same multi-stage compilation process that happens on a desktop GHC installation. The difference is that every stage runs inside the Wasm sandbox.

## What This Means for xeus-haskell

At a high level, the architecture of GHC in Browser appears to map surprisingly well onto the xeus-haskell problem. Both need:

- A GHC runtime compiled to WebAssembly
- A virtual filesystem with the standard libraries and package database
- JavaScript glue to bridge browser APIs and the Wasm runtime
- Async handling so the UI stays responsive during compilation

![Working module diagram](/images/8.working%20module.png)

The most obvious difference is the communication protocol. GHC in Browser talks directly to a web UI, while xeus-haskell needs to speak the Jupyter messaging protocol so it can plug into JupyterLite as a proper kernel. But I suspect there are less obvious differences too — notebook kernels need to manage persistent state across cells, handle interrupts gracefully, and deal with richer output types than plain text. Those are problems GHC in Browser doesn't have to solve, and I don't yet know how cleanly the current architecture will accommodate them.

That said, studying GHC in Browser gave me a much more concrete understanding of each layer — what it does, why it's needed, and where I should expect friction. Before this investigation, my mental model of "GHC in a browser" was fairly hand-wavy. Now I have a clearer sense of which pieces I can likely reuse and which ones I'll need to build or adapt from scratch. That's the foundation I'm working from as I start the xeus-haskell implementation this summer.

## Next Steps

If this kind of work interests you, here's where to look:

- **[GHC in Browser](https://petertrsko.github.io/ghc-wasm-haskell-playground/)** — try it yourself, compile some Haskell in your browser tab.
- **[xeus-haskell](https://github.com/IHaskell/xeus-haskell)** — the Jupyter kernel this work feeds into. Contributions welcome.
- **[GHC WASM backend](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html)** — the GHC documentation on the WebAssembly backend.
- **[WASI specification](https://wasi.dev/)** — if you want to understand the system interface layer in depth.

## Conclusion

This exploration gave me a much clearer picture of what is required to bring full GHC support to browser-based notebook environments — and an honest sense of which parts I still don't fully understand. The next step is applying these lessons to xeus-haskell and finding out how much of this architecture can actually be reused within a JupyterLite kernel versus how much will need to be rethought.

I'll be posting updates as the GSoC project progresses. If you've worked with GHC WASM, WASI, or browser-based compiler tooling and have thoughts, I'd genuinely appreciate hearing from you.

*Arman Sanjay Choudhary*
