# 7. Benchmarking and optimization

In this part, we'll add a macro for benchmarking the interpreter and apply a set of optimizations to improve its performance.

## Setup
To try it out; simply download and install [Swift](https://www.swift.org/download/) and run `swift run` from this directory.

## Benchmarking
We'll add a new macro that measures the time it takes to evaluate it's argument in a loop.

```
1. benchmark 10 sleep milliseconds 100
2. 
1.001935008 seconds
```

## Optimizations
We will use the recursive and tail recursive Fibonacci functions seen previously to evaluate the impact of our optimizations.

```
function fib1(n:Int):Int
  if n.< 2 n else (fib1 n.- 1).+ fib1 n.- 2
  
benchmark 100 fib1 20
```

```
function fib2(n:Int a:Int b:Int):Int
  if n.> 1 return fib2 n.- 1 b a.+ b else if n.= 0 a else b

benchmark 10000 fib2 70 0 1
```

### Convert call stack to embedded linked list
We started out using the most obvious implementation possible for a call stack: an array of call frames. 

```
struct Function {
    struct Call {
	let position: Position
        let returnPc: PC
        let stackOffset: Int
        let target: Function
        
        init(_ target: Function, at pos: Position, stackOffset: Int, returnPc: PC) {
            self.target = target
	    self.position = pos
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
        vm.callStack.append(Function.Call(f, at: pos, stackOffset: vm.stack.count-fargs.count, returnPc: vm.pc))
	...
    }
    ...
}
```

Converting it to an embedded linked list, where each frame contains an optional reference to its parent; gives us a 30 performance boost.

```
struct Function {
    class Call {
        let parentCall: Call?
	...
        
        init(_ parentCall: Call?, ...) {
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
                                       at: pos, stackOffset: vm.stack.count-fargs.count, returnPc: vm.pc)
	...
    }
    ...
}
```

### Pass program counter and stack as arguments
The program counter and the stack are both frequently updated during evaluation, so it makes sense to make sure access is as fast as possible. Inititally we put the them inside the `VM`, and later moved both to the `Task` class. 

```
class VM {
    var pc: PC {
        get {currentTask!.pc}
        set(pc) {currentTask!.pc = pc}
    }

    var stack: Stack {
        get {currentTask!.stack}
        set(v) {currentTask!.stack = v}
    }
    
    func eval(fromPc: PC) throws {
        pc = fromPc

        loop: while true {
            switch op {
            case let .push(value):
                push(value)
		...
	    }
	}
    }

    func pop() -> Value {
        stack.removeLast()
    }

    func push(_ value: Value) {
        stack.append(value)
    }
}
```

Passing the current program counter and stack as arguments instead of manipulating them via the current task gives us a 75% performance boost.

```
class VM {
    func eval(fromPc: PC, stack: inout Stack) throws {
        var pc = fromPc

        loop: while true {
            switch op {
            case let .push(value):
                stack.push(value)
		...
	    }
	}
    }
}

struct Function: CustomStringConvertible {
    typealias Body = (Function, VM, inout PC, inout Stack, Position) throws -> Void
    ...
}

class StandardLibrary: Namespace {
    init() {
        bindFunction("yield", [], nil) {(_, vm, pc, stack, pos) throws in
            let c1 = vm.currentTask!
            c1.pc = pc
            c1.stack = stack
            vm.tasks.append(vm.tasks.removeFirst())
            let c2 = vm.currentTask!
            pc = c2.pc
            stack = c2.stack
        }
    }
}
```

### Implement tail calls
A tail call is a call of a user defined function directly followed by a return, which allows reusing the current call frame rather than pushing a new one. We'll add a `return` macro that triggers tail calls to be emitted when expanded with a user defined function call as argument. This gives the tail recursive benchmark a 60% performance boost.

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

### Move type checks from runtime to compilation
For simplcitys sake, function argument type checks were initially performed at runtime when calling the function.

```
struct Function: CustomStringConvertible {
    ...
    func call(_ vm: VM, at pos: Position) throws {
        ...
        for i in 0..<arguments.count {
            let expected = arguments[i].1
            let actual  = vm.stack[vm.stack.count - arguments.count + i].type

            if !actual.equals(expected) {
                throw EvalError.typeMismatch(pos, expected, actual)
            }
        }
	...
    }
}
```

But in many cases we actually have enough information to perform the check at compile time.
Where runtime checks are needed, they are now emitted as separate operations per argument.
This gives us a 20% performance boost.

```
class FunctionType: BasicValueType<Function> {
    ...
    override func identifierEmit(_ value: Value,
                                 _ vm: VM,
                                 at pos: Position,
				 inNamespace ns: Namespace,
				 withArguments args: inout [Form],
				 options opts: Set<EmitOption>) throws {
	let f = try cast(value, at: pos)

	for a in f.arguments {
	    if args.isEmpty {
		throw EmitError.missingArgument(pos)
	    }

	    let f = args.removeFirst()
	    let expected = a.1
	    var actual: (any ValueType)?

	    if f is Literal {
		actual = (f as! Literal).value.type
	    } else if f is Identifier {
		let v = ns[(f as! Identifier).name]

		if v != nil && v!.type.equals(std.argumentType) {
		    actual = try std.argumentType.cast(v!, at: f.position).1
		}
	    }

	    if actual != nil {
		if !actual!.equals(expected) {
		    throw EmitError.typeMismatch(f.position, expected, actual!)
		}
	    }

	    try f.emit(vm, inNamespace: ns, withArguments: &args, options: [])
	    if actual == nil { vm.emit(.checkType(f.position, expected)) }
	}
	...
    }
}

class VM {
    ...
    func eval(fromPc: PC) throws {
        ...
        loop: while true {
	    ...            
            switch op {
            case let .checkType(pos, expected):
                let actual = stack.last!.type
                if !actual.equals(expected) { throw EvalError.typeMismatch(pos, expected, actual) }
                pc += 1
	    }
	    ...
	}
    }
}
```