## introduction
The goal here is to provide the foundation for an interpreter in Swift that is simple enough to understand,
yet substantial enough to continue building upon and playing around with.<br/>
<br/>
A taste of interpreters, Swift, or both; depending on where you're coming from.<br/>
<br/>
Rather than starting with parsing; we'll work from the bottom up, starting with the virtual machine and gradually adding syntax on top.

## motivation
Because I consider learning how our tools are constructed is an essential part of becoming a software developer.<br/>
<br/>
In addition, interpreters are useful in many situations; configuration languages, expression evaluators, template languages and scripting languages just to name a few.

## setup
To try it out; simply download and install [Swift](https://www.swift.org/download/), `cd` into a version and run `swift main.swift`.

## design
The virtual machine we'll build is stack based, as opposed to register based.<br/>
<br/>
Each design has it's strengths and weaknesses; stacks need to be shuffled and registers allocated; but I find stacks more elegant and easy to reason about.

## versions

### v1
Implement a minimum viable virtual machine with a stack and primitive functions.

### v2
Add tracing for debugging purposes and tasks, aka. fibers or green threads.

### v3
Add namespaces, forms, macros and improve error handling.

### v4
Add call stack and user defined functions.

### v5
Add readers and REPL.

### v6
Add the missing pieces to implement two flavors of Fibonacci.

### v7
Add support for benchmarking and implement optimizations.