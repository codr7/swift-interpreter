/* Version 2 */

/*
 We'll use structs to represent values, every value has a type.
 */

struct Value {
    let type: ValueType
    let data: Any

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

class ValueType {
    let name: String
    
    init(_ name: String) {
        self.name = name
    }
}

/*
 We'll use a type alias to make it clear when we're dealing with a program counter,
 as opposed to any random integer.
 */

typealias PC = Int

/*
 Functions take the current program counter as a reference argument when called,
 which allows them to decide where to return to.

 This time around; we'll only deal with primitives, which typically simply increase the program counter.
 
 But once we get to user defined functions, this allows us to jump to the actual code of the called function.
 */

struct Fun {
    typealias Body = (VM, inout PC) throws -> Void
    
    let name: String
    let body: Body

    init(_ name: String, _ body: @escaping Body) {
        self.name = name
        self.body = body
    }

    func call(_ vm: VM, pc: inout PC) throws {
        try body(vm, &pc)
    }
}

/*
 Operations are the things that our virtual machine executes.

 Any kind of code we want to run on it, regardless of syntax; needs to be reduced to a sequence of operations.

 The reason there's a separate case for stopping is to avoid having to check in the eval loop,
 which needs to be as fast as possible.
 */

enum Op {
    case call(Fun)
    case push(Value)
    case stop
    case trace
    case yield
}

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

/*
 The virtual machine is where the rubber finally meets the road.
 */

class VM {
    var trace = false
    var code: [Op] = []
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
    
    func dumpStack() -> String {
        "[\(currentTask!.stack.map({$0.dump()}).joined(separator: " "))]"
    }

    func emit(_ op: Op) {
        if trace { code.append(.trace) }
        code.append(op)
    }
    
    func eval(fromPc: PC) throws {
        pc = fromPc
        
        loop: while true {
            let op = code[pc]
 
            switch op {
            case let .call(target):
                try target.call(self, pc: &pc)
            case let .push(value):
                push(value)
                pc += 1
            case .stop:
                if tasks.count == 1 {
                    break loop
                } else {
                    tasks.removeFirst()
                }
            case .trace:
                pc += 1
                print("\(pc) \(code[pc])")
            case .yield:
                pc += 1
                tasks.append(tasks.removeFirst())
            }
        }
    }

    func pop() -> Value? {
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

/*
 The humble beginnings of a standard library.
 */

let intType = ValueType("Int")

let addFun = Fun("+") {(vm: VM, pc: inout PC) throws -> Void in
    let r = vm.pop()!
    let l = vm.pop()!
    vm.push(Value(intType, (l.data as! Int) + (r.data as! Int)))
    pc += 1
}

/*
 Now we're ready to take it for a spin.

 We'll start an extra task, in addition to the main task, and yield between them a few times.
   */

let pingFun = Fun("ping") {(vm: VM, pc: inout PC) throws in
    print("ping \(vm.currentTask!.id)")
    pc += 1
}

let pongFun = Fun("pong") {(vm: VM, pc: inout PC) throws in
    print("pong \(vm.currentTask!.id)")
    pc += 1
}

let vm = VM()
vm.trace = true
vm.emit(.call(pingFun))
vm.emit(.yield)
vm.emit(.call(pongFun))
vm.emit(.yield)
vm.emit(.stop)
vm.startTask()
try vm.eval(fromPc: 0)

/*
 Output:

1 call(main.Fun)
ping 0
3 yield
1 call(main.Fun)
ping 1
3 yield
5 call(main.Fun)
pong 0
7 yield
5 call(main.Fun)
pong 1
7 yield
9 stop
 */
