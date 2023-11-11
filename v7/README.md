## Introduction
This time around we'll add some tools for benchmarking the interpreter and apply a set of optimizations to improve its performance.

## Benchmarking
We'll add a new macro that measures the time it takes to evaluate it's argument in a loop.

```
1. function fib1(n) if < n 2 n else + fib1 - n 1 fib1 - n 2
2. benchmark 10 fib1 20
3.
0.42437844 seconds
```

## Optimizations
We will reuse the recursive and tail recursive Fibonacci functions seen previously to evaluate our optimizations:

```
function fib1(n) if < n 2 n else + fib1 - n 1 fib1 - n 2
function fib2(n a b) if > n 1 return fib2 - n 1 b + a b else if = n 0 a else b
```

### Convert call stack to embedded linked list
We started out using the most obvious implementation possible for a call stack: an array of call frames. 

```
struct Function {
    struct Call {
        let target: Function
        let stackOffset: Int
        let returnPc: PC
        
        init(_ target: Function, stackOffset: Int, returnPc: PC) {
            self.target = target
            self.stackOffset = stackOffset
            self.returnPc = returnPc
        }
    }
}

class VM {    
    var callStack: [Function.Call] {
        get {currentTask!.callStack}
        set(v) {currentTask!.callStack = v} 
    }
    ...
}

func eval(fromPc: PC) throws {
    ...
    loop: while true {
        ...
        switch op {
        case let .popCall(target):
            let c = vm.callStack.removeLast()
	    ...
	...
	}
    }
}

stdMacro("function") {(_, vm, pos, ns, args) throws in
    ...
    let f = Function(id, fargs) {(f, vm) throws in
        vm.callStack.append(Function.Call(f, stackOffset: vm.stack.count-fargs.count, returnPc: vm.pc))
	...
    }
    ...
}
```

Converting it to an embedded linked list, where each call containes an optional reference to the previous call; gives a 30-40% boost.

```
struct Function {
    class Call {
        let parentCall: Call?
	...
        
        init(_ parentCall: Call?, _ target: Function, stackOffset: Int, returnPc: PC) {
            self.parentCall = parentCall
	    ...
        }
    }
    ...
}

class VM {        
    var currentCall: Function.Call? {
        get {currentTask!.currentCall}
        set(v) {currentTask!.currentCall = v} 
    }
    ...
}

func eval(fromPc: PC) throws {
    ...
    loop: while true {
        ...
        switch op {
        case let .popCall(target):
           let c = vm.currentCall!
           vm.currentCall = c.parentCall
	    ...
	...
	}
    }
}

stdMacro("function") {(_, vm, pos, ns, args) throws in
    ...
    let f = Function(id, fargs) {(f, vm) throws in
        vm.currentCall = Function.Call(vm.currentCall, f,
                                       stackOffset: vm.stack.count-fargs.count, returnPc: vm.pc)
	...
    }
    ...
}
```

## Add support for tail calls
A tail call is a call of a user defined function directly followed by a return, which allows reusing the current call frame rather than pushing a new one. We'll add a `return` macro that triggers tail calls to be emitted when expanded with a user defined function call as argument. This gives the tail recursive version of Fibonacci a 60% boost.

```
enum EmitOption  {
    case returning
}

class FunctionType: ValueType {
    ...
    override func identifierEmit(_ value: Value, _ vm: VM, at pos: Position,
                                 inNamespace ns: Namespace, withArguments args: inout [Form],
                                 options opts: Set<EmitOption>) throws {
	...
        if opts.contains(.returning) && f.startPc != nil {
            vm.emit(.tailCall(pos, f))
        } else {
            vm.emit(.call(pos, f))
        }
    }
}

stdMacro("return") {(_, vm, pos, ns, args) throws in
    try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args, options: [.returning])
}

func eval(fromPc: PC) throws {
    ...
    loop: while true {
        ...
        switch op {	
        case let .tailCall(pos, target):
            let c = vm.currentCall
                
            if c == nil || c!.target.startPc == nil {
                pc += 1
                try target.call(self, at: pos)
            } else {
                c!.target = target
                c!.stackOffset = vm.stack.count - target.arguments.count
                pc = target.startPc!
            }
	...
	}
    }
}
```