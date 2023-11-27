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

/* Tasks represent independent flows of execution with separate stacks and program counters. */

class Task {
    typealias Id = Int

    let id: Id
    var stack: Stack = []
    var pc: PC

    init(id: Id, startPc: PC) {
        self.id = id
        self.pc = startPc
    }
}

class VM {
    enum Operation {
        case call(Function)
        case push(Value)
        case stop
        case trace
    }

    var trace = false
    var code: [Operation] = []
    var nextTaskId = 0
    var tasks: [Task] = []
    var currentTask: Task? {tasks[0]}
    
    var pc: PC {
        get {currentTask!.pc}
        set(pc) {currentTask!.pc = pc}
    }

    init() {
        startTask()
    }
    
    func emit(_ op: Operation) {
        if trace { code.append(.trace) }
        code.append(op)
    }
    
    func evaluate(fromPc: PC) throws {
        pc = fromPc
        
        loop: while true {
            let op = code[pc]
 
            switch op {
            case let .call(target):
                pc += 1
                try target.call(self)
            case let .push(value):
                push(value)
                pc += 1
            case .stop:
                pc += 1
                break loop
            case .trace:
                pc += 1
                print("\(pc) \(code[pc])")
            }
        }
    }

    func pop() -> Value {
        currentTask!.stack.removeLast()
    }

    func push(_ value: Value) {
        currentTask!.stack.append(value)
    }

    func startTask(pc: PC = 0) {
        let t = Task(id: nextTaskId, startPc: pc)
        tasks.append(t)
        nextTaskId += 1
    }
}

let intType = ValueType("Int")

let addFunction = Function("+") {(_, vm) throws in
    let r = vm.pop()
    let l = vm.pop()
    vm.push(Value(intType, (l.data as! Int) + (r.data as! Int)))
}

/*
 Now we're ready to take it for a spin.

 We'll start an extra task, in addition to the main task, and yield between them a few times.
   */

let yieldFunction = Function("yield") {(_, vm) throws in
    vm.tasks.append(vm.tasks.removeFirst())
}

let pingFunction = Function("ping") {(_, vm) throws in
    print("ping \(vm.currentTask!.id)")
}

let pongFunction = Function("pong") {(_, vm) throws in
    print("pong \(vm.currentTask!.id)")
}

let vm = VM()
vm.trace = true
vm.emit(.call(pingFunction))
vm.emit(.call(yieldFunction))
vm.emit(.call(pongFunction))
vm.emit(.call(yieldFunction))
vm.emit(.stop)
vm.startTask()
try vm.evaluate(fromPc: 0)

/*
 Output:

1 call(main.Function(name: "ping", body: (Function)))
ping 0
3 yield
1 call(main.Function(name: "ping", body: (Function)))
ping 1
3 yield
5 call(main.Function(name: "pong", body: (Function)))
pong 0
7 yield
5 call(main.Function(name: "pong", body: (Function)))
pong 1
7 yield
9 stop
9 stop
 */
