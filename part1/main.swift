struct Value: CustomStringConvertible {
    let data: Any
    let type: ValueType
    
    var description: String { type.toString(self) }

    init(_ type: ValueType, _ data: Any) {
        self.type = type
        self.data = data
    }
}

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

typealias PC = Int
typealias Stack = [Value]

class VM {
    enum Operation {
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

let intType = ValueType("Int")

let addFunction = Function("+") {(_, vm) throws in
    let r = vm.pop()
    let l = vm.pop()
    vm.push(Value(intType, (l.data as! Int) + (r.data as! Int)))
}

let vm = VM()
vm.emit(.push(Value(intType, 6)))
vm.emit(.push(Value(intType, 4)))
vm.emit(.call(addFunction))
vm.emit(.stop)
try vm.evaluate(fromPc: 0)
print(vm.pop())
