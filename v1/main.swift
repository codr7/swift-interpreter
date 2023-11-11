/*
 Version 1
 */

/*
 We use structs to represent values, every value has a type.
 */

struct Value: CustomStringConvertible {
    let type: ValueType
    let data: Any
    
    var description: String { type.dump(self) }

    init(_ type: ValueType, _ data: Any) {
        self.type = type
        self.data = data
    }

    func dump() -> String {
        "\(data)"
    }
}

/*
 Types don't do much yet, but may be used to specialize behavior for certain kinds of values.
 */

class ValueType: CustomStringConvertible {
    let name: String
    var description: String { name }

    init(_ name: String) {
        self.name = name
    }

    func dump(_ value: Value) -> String {
        "\(value.data)"        
    }    
}

/*
 Functions can be either primitive or user defined.

 Primitive functions are implemented in Swift inside the body, while user defined functions are implemented as virtual operations and use the call stack.

 We'll only deal with primitives for now.
 */

struct Function: CustomStringConvertible {
    typealias Body = (Function, VM) throws -> Void
    
    let name: String
    let body: Body

    var description: String { name }

    init(_ name: String, _ body: @escaping Body) {
        self.name = name
        self.body = body
    }

    func call(_ vm: VM) throws {
        try body(self, vm)
    }
}

/*
 Operations are the things that our virtual machine executes.

 Any kind of code we want to run on it, regardless of syntax; needs to be reduced to a sequence of operations.

 The reason there's a separate case for stopping is to avoid having to check in the eval loop,
 which needs to be as fast as possible.
 */

enum Op {
    case call(Function)
    case push(Value)
    case stop
}

typealias PC = Int
typealias Stack = [Value]

/*
 The virtual machine is where the rubber finally meets the road.
 */

class VM {
    var code: [Op] = []
    var stack: Stack = []

    func emit(_ op: Op) {
        code.append(op)
    }
    
    func eval(fromPc: PC) throws {
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

/*
 The humble beginnings of a standard library.
 */

let intType = ValueType("Int")

let addFunction = Function("+") {(_, vm) throws in
    let r = vm.pop()
    let l = vm.pop()
    vm.push(Value(intType, (l.data as! Int) + (r.data as! Int)))
}

/*
 Now we're ready to take it for a spin.

 We'll settle for some simple arithmetics this time around, just to get an idea how everything works.
 */

let vm = VM()
vm.emit(.push(Value(intType, 6)))
vm.emit(.push(Value(intType, 4)))
vm.emit(.call(addFunction))
vm.emit(.stop)
try vm.eval(fromPc: 0)

/*
 This prints 10, which is the final contents of the stack after adding 6 to 4.
 */

print(vm.pop())
