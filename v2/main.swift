/***
 The goal here is to provide the foundation for an interpreter in Swift that is simple enough to understand,
 yet substantial enough to continue building upon and playing around with.

 A taste of interpreters, Swift, or both; depending on where you're coming from.

 Rather than starting with syntax and parsers, we'll work from the bottom up.
  
 To try it out; simply download and install Swift, and run `swift main.swift`.

 CHANGES
 -------
 v2:
 Added support for tasks, aka. fibers or green threads.
 ***/

/*
 First out is types and values.
 Types don't do much yet, but may be used to specialize behavior for certain kinds of values.
 */

class T {
    let name: String
    
    init(_ name: String) {
        self.name = name
    }
}

struct V {
    let type: T
    let data: Any

    init(_ type: T, _ data: Any) {
        self.type = type
        self.data = data
    }

    func dump() -> String {
        "\(data)"
    }
}

/*
 We'll use a type alias to make it clear when we're dealing with a program counter,
 as opposed to any random integer.
 */

typealias Pc = Int

/*
 Functions take the current program counter as a reference argument when called,
 which allows them to decide where to return to.

 This time around; we'll only deal with primitives, which typically simply increase the program counter.
 
 But once we get to user defined functions, this allows us to jump to the actual code of the called function.
 */

class Fun {
    typealias Body = (M, Pc) throws -> Pc
    
    let name: String
    let body: Body

    init(_ name: String, _ body: @escaping Body) {
        self.name = name
        self.body = body
    }

    func call(_ m: M, pc: Pc) throws -> Pc {
        try body(m, pc)
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
    case push(V)
    case stop
    case yield
}

/* Tasks represent independent flows of execution with separate stacks and program counters. */

class Task {
    typealias Id = Int

    let id: Id
    var stack: [V] = []
    var pc: Pc

    init(id: Id, startPc: Pc) {
        self.id = id
        self.pc = startPc
    }
}

/*
 The virtual machine is where the rubber finally meets the road.
 */

class M {
    var code: [Op] = []
    var nextTaskId = 0
    var tasks: [Task] = []
    var currentTask: Task? {tasks[0]}
    var pc: Pc {
        get {currentTask!.pc}
        set(pc) {currentTask!.pc = pc}
    }

    init() {
        startTask()
    }
    
    func dumpStack() async -> String {
        "[\(currentTask!.stack.map({$0.dump()}).joined(separator: " "))]"
    }

    func emit(_ op: Op) {
        code.append(op)
    }
    
    func eval(fromPc: Pc) throws {
        pc = fromPc
        
        loop: while true {
            let op = code[pc]
 
            switch op {
            case let .call(target):
                try pc = target.call(self, pc: pc)
            case let .push(v):
                push(v)
                pc += 1
                break
            case .stop:
                break loop
            case .yield:
                pc += 1
                switchTask()
                try! eval(fromPc: m.currentTask!.pc)
                break
            }
        }
    }

    func pop() -> V? {
        currentTask!.stack.removeLast()
    }

    func push(_ v: V) {
        currentTask!.stack.append(v)
    }

    func startTask(pc: Pc = 0) {
        let t = Task(id: nextTaskId, startPc: pc)
        tasks.append(t)
        nextTaskId += 1
    }

    func switchTask() {
        tasks.append(tasks.removeFirst())
    }
}

/*
 Now we're ready to take it for a spin.

 We'll let two tasks play ping pong.
 */

let m = M()

let pingFun = Fun("ping") {(m: M, pc: Pc) throws -> Pc in
    print("ping \(m.currentTask!.id)")
    return pc + 1
}

let pongFun = Fun("pong") {(m: M, pc: Pc) throws -> Pc in
    print("pong \(m.currentTask!.id)")
    return pc + 1
}

m.emit(.call(pingFun))
m.emit(.yield)
m.emit(.call(pongFun))
m.emit(.yield)
m.emit(.stop)

m.startTask()
try m.eval(fromPc: 0)

/*
 Output:

 ping 0
 ping 1
 pong 0
 pong 1
 */
