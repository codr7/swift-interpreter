# 1. A minimum viable virtual machine

## Setup
To try it out; simply download and install [Swift](https://www.swift.org/download/) run `swift main.swift` from this directory.

## Welcome to the machine
The virtual machine we'll build is stack based, as opposed to register based.<br/>
<br/>
Each design has it's strengths and weaknesses; stacks need to be shuffled and registers allocated; but I find stacks more elegant and easy to reason about.br/>
<br/>
We'll use an `enum` to represent VM operations.<br/>
<br/>
Any kind of code we want to run, regardless of syntax; will eventually be reduced to a sequence of operations.<br/>
<br/>
The reason there's a separate case for stopping is to avoid having to check in the evaluation loop,
which needs to be as fast as possible.

```swift
typealias PC = Int
typealias Stack = [Value]

class VM {
    enum Op {
        case call(Function)
        case push(Value)
        case stop
    }
    
    var code: [Operation] = []
    var stack: Stack = []

    func emit(_ op: Operation) {
        code.append(op)
    }
    
    func evaluate(fromPc: PC) throws {
        var pc = fromPc
        
        loop: while true {
            let op = code[pc]
 
            switch op {
            case let .call(target):
                pc += 1
                try target.call(self)
            case let .push(v):
                stack.append(v)
                pc += 1
            case .stop:
	        pc += 1
                break loop
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

## Types and values
We'll use structs to represent values, every value has a type.

```swift
struct Value: CustomStringConvertible {
    let data: Any
    let type: ValueType

    var description: String { type.dump(self) }

    init(_ type: ValueType, _ data: Any) {
        self.type = type
        self.data = data
    }
}
```

Types are used to specialize behavior behavior for certain kinds of values.

```swift
class ValueType: CustomStringConvertible {
    let name: String
    var description: String { name }

    init(_ name: String) {
        self.name = name
    }

    func toString(_ value: Value) -> String {
        "\(value.data)"        
    }    
}
```

## Functions
Functions can be either primitive or user defined.<br/>
<br/>
Primitive functions are implemented in Swift, while user defined functions are implemented as a sequence of VM operations.<br/>
<br/>
We'll only deal with primitives for now.

```swift
struct Function: CustomStringConvertible {
    typealias Body = (Function, VM) throws -> Void
    
    let body: Body
    let name: String

    var description: String { name }

    init(_ name: String, _ body: @escaping Body) {
        self.name = name
        self.body = body
    }

    func call(_ vm: VM) throws {
        try body(self, vm)
    }
}
```

## Standard library
The humble beginnings of a standard library, an integer type and a function to perform addition.

```swift
let intType = ValueType("Int")

let addFunction = Function("+") {(_, vm) throws in
    let r = vm.pop()
    let l = vm.pop()
    vm.push(Value(intType, (l.data as! Int) + (r.data as! Int)))
}
```

## Showtime
This prints 10, which is result of adding 6 to 4.

```swift
let vm = VM()
vm.emit(.push(Value(intType, 6)))
vm.emit(.push(Value(intType, 4)))
vm.emit(.call(addFunction))
vm.emit(.stop)
try vm.evaluate(fromPc: 0)
print(vm.pop())
```