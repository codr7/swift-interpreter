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