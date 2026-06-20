---
layout: post
title:  "Evaluating GHC WASM for Browser-Based Haskell Execution"
date:   2026-06-20 12:56:00 +0530
categories: blog
---



**GHC in the Browser**

*How WebAssembly makes a full Haskell compiler run client-side*

Arman Sanjay Choudhary · GSoC 2026 · Haskell.org

# **Introduction: Exploring GHC in the Browser**

As part of my Google Summer of Code 2026 project with Haskell.org, I am working on integrating a GHC/WebAssembly backend into **xeus-haskell** — a Jupyter kernel for Haskell that currently runs on MicroHs. My task is to implement a Wasm-based GHC kernel so that xeus-haskell can support the full GHC language and a broader set of Hackage packages inside JupyterLite.

Before building it, I studied **GHC in Browser** — an existing project that already runs GHC entirely in the browser using WebAssembly. Understanding how it works is the most direct preparation for the implementation work ahead.

This article documents what I learned: how the major components connect, how code moves through the system, and which design choices make browser-native GHC execution possible. It is not a deep implementation guide — it is a system-level walkthrough written to inform the xeus-haskell integration.

## **Motivation**

The main motivation is to make Haskell easier to access. Today, using Haskell interactively usually requires either a server running in the background or a local GHC installation. Both can make it harder for new users to get started.

Running Haskell directly in the browser changes that. A student can open a URL and start writing code immediately without installing anything. This makes Haskell easier to use in classrooms, tutorials, online playgrounds, and browser-based notebooks.

It also has some practical benefits:

* Easier for beginners to get started.  
* Useful for education and interactive learning.  
* Works well with browser-based notebooks such as JupyterLite.  
* Reduces the need for dedicated server infrastructure.  
* Benefits from the browser's built-in security sandbox.  
* Can continue working offline after the initial download.

These are some of the reasons why browser-native Haskell is interesting and why understanding projects like **GHC in Browser** is useful for future work on **xeus-haskell**.

**1\. Understanding GHC in Browser** 

Before examining implementation details, it is useful to understand the system from a high level.

From a user's perspective, the workflow is straightforward:

1. Open the web application.  
2. Write Haskell code.  
3. Press Run.  
4. Receive the result.

Internally, several components cooperate to make this possible.

![Understanding GHC in Browser](/images/1.Understanding%20Ghc%20browser%20diagram.png)

The browser hosts both the user interface and the compiler runtime. Supporting infrastructure such as a virtual filesystem and WebAssembly runtime provides the environment that GHC expects during compilation and execution. 

**1.1 Traditional vs Browser-Native Execution**

Historically, most online programming environments followed a server-based architecture. 

![Traditional vs native execution](/images/2.traditional%20vs%20native%20diag.png)

The browser acted primarily as a client while compilation and execution occurred on remote infrastructure.

In contrast, GHC in Browser moves the execution environment into the browser itself.

![Browser-native execution](/images/3.native%20diag.png)

After the application has loaded, code compilation and execution occur locally within the user's browser session.

This architectural shift changes where computation occurs, reduces dependence on remote infrastructure, and provides a useful foundation for browser-native development tools.

# **1.2 Architectural Questions**

While exploring the project, I focused on four questions:

1. How is GHC compiled to WebAssembly?  
2. How does the browser provide the environment expected by GHC?  
3. How are compiled artifacts loaded and executed?  
4. How does the system remain responsive during compilation?

The remainder of this article examines the components involved in answering these questions.

**2\. Architecture Overview** 

The system can be viewed as four major layers. 

![Architecture overview](/images/4.architecure%20diag.png)

Each layer has a distinct responsibility: 

| Layer | Responsibility |
| :---- | :---- |
| **User Interface**  | Code editing and result presentation |
| **JavaScript Runtime**  | Loading modules and coordinating execution |
| **GHC WebAssembly Runtime**  | Interpreted and program execution |
| **Browser Platform**  | WebAssembly execution and browser APIs  |

**2.1 The Role of WebAssembly** 

At the core of the system is WebAssembly (Wasm), a portable binary format designed to run efficiently across different platforms. Instead of interpreting GHC for a traditional operating system, the interpreter is built for the WebAssembly target and executed inside the browser.

This allows the browser to host a complete Haskell toolchain without requiring a remote server. Once the required resources have been downloaded, execution happens locally on the user's machine.

![WebAssembly execution model](/images/5.webassembly%20diag.png)

WebAssembly provides the execution environment, while JavaScript supplies the browser-specific functionality needed by the interpreter. 

**2.3 The Role of WASI** 

GHC expects access to features normally provided by an operating system, including files, directories, and standard input/output streams. Browsers do not provide these interfaces directly.

To bridge this gap, GHC in Browser relies on the WebAssembly System Interface (WASI). WASI provides a standard set of APIs that allow WebAssembly programs to interact with their environment in a controlled and portable manner.

![WASI working diagram](/images/6.wasi%20working.png)

Using WASI, the browser can provide many of the services that GHC expects while still maintaining the browser's security model. 

**3\. Execution Flow** 

Once the environment has been initialized, the execution process follows a predictable sequence. 

## **3.1 From Code to Result**

When a user presses **Run**, the browser sends the source code to the GHC runtime for compilation and evaluation.

![Execution flow](/images/7.execution%20flow.png)

From the user's perspective, this appears as a simple request-and-response workflow. Internally, however, the code passes through several compilation stages before the final result is produced.

# 

# **3.2 Why a Virtual Filesystem is Needed**

A traditional GHC installation relies heavily on files stored on disk. Standard libraries, package metadata, and temporary compilation artifacts are all accessed through the filesystem.

Because browsers do not provide a traditional filesystem, GHC in Browser creates a virtual filesystem that mimics the structure expected by the compiler.

The virtual filesystem contains:

* GHC runtime resources  
* Standard Haskell libraries  
* Package information  
* Temporary files used during compilation

From the compiler's perspective, this environment behaves similarly to a normal local installation.

# **4\. Relevance to xeus-haskell**

The ideas explored in GHC in Browser are particularly relevant to the ongoing work on xeus-haskell. A browser-compatible GHC runtime could eventually allow notebook environments to execute Haskell code locally without depending on a remote execution service.

![Working module diagram](/images/8.working%20module.png)

While the implementation details will differ, many of the same architectural concepts—WebAssembly execution, filesystem virtualization, and browser-based runtime management—are directly applicable. 

### **5\. Key Takeaways**

* GHC in Browser demonstrates that a full GHC toolchain can run inside a web browser using WebAssembly.  
* The system combines multiple components, including a browser interface, JavaScript runtime, GHC WebAssembly runtime, and a virtual filesystem.  
* WebAssembly provides the execution environment, while WASI and browser APIs help supply the services that GHC normally expects from an operating system.  
* After the initial application loads, compilation and execution take place locally in the user's browser rather than on a remote server.  
* The architecture provides useful ideas for future browser-based Haskell tools, including notebook environments built on xeus-haskell and JupyterLite.

# 

# **6\. Conclusion**

GHC in Browser is an interesting example of how modern WebAssembly technology can move traditionally server-side development tools into the browser. By combining a WebAssembly-compiled version of GHC with supporting runtime infrastructure, the project makes it possible to compile and run Haskell code locally within a browser session.

For my GSoC project, this exploration served as a foundation for understanding how a browser-native Haskell environment can be built. Many of the ideas used in GHC in Browser—such as WebAssembly execution, virtual filesystem support, and browser-based runtime management—are directly relevant to the planned integration of a GHC/WebAssembly backend into xeus-haskell.

While the implementation details will differ, studying GHC in Browser provided valuable insight into the architectural challenges and design decisions involved in bringing the Haskell ecosystem closer to browser-based notebooks and interactive computing environments. As WebAssembly continues to mature, projects like these may help make Haskell tooling more accessible to students, educators, and developers without requiring complex local installations or dedicated server infrastructure.
