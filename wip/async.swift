/***
 The goal here is to provide the foundation for an interpreter in Swift that is simple enough to understand,
 yet substantial enough to continue building upon and playing around with.

 A taste of interpreters, Swift, or both; depending on where you're coming from.

 Rather than starting with syntax and parsers, we'll work from the bottom up.
 
 To try it out; simply download and install Swift, and run `swift main.swift`.

TODO:
* add .yield op
 ***/

/*
 First out is types and values.

 Types don't do much yet, but may be used to specialize behavior for certain kinds of values.
 */

class ValType {
    let name: String

    init(_ name: String) {
        self.name = name
    }
}

struct Val {
    let type: ValType
    let data: Any

    init(_ type: ValType, _ data: Any) {
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
    typealias Body = (M, Pc) async throws -> Pc
    
    let name: String
    let body: Body

    init(_ name: String, _ body: @escaping Body) {
        self.name = name
        self.body = body
    }

    func call(_ m: M, pc: Pc) async throws -> Pc {
        try await body(m, pc)
    }
}

/* Tasks represent independent flows of execution. */

actor Task {
    let m: M
    var stack: [Val] = []
    var pc: Pc

    init(_ m: M, pc: Pc) {
        self.m = m
        self.pc = pc
    }

    func eval(fromPc: Pc) async throws {
        pc = fromPc
        
        loop: while true {
            let op = await m.code[pc]
 
            switch op {
            case let .call(target):
                try await pc = target.call(m, pc: pc)
            case let .push(val):
                stack.append(val)
                pc += 1
                break
            case .stop:
                break loop
            }
        }
    }

    func pop() -> Val? {
        stack.removeLast()
    }

    func push(_ val: Val) {
        stack.append(val)
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
    case push(Val)
    case stop
}

/*
 The virtual machine is where the rubber finally meets the road.
 */

actor M {
    var code: [Op] = []
    var currentTask: Task?
    var tasks: [Task] = []

    func dumpStack() async -> String {
        "[\(await currentTask!.stack.map({$0.dump()}).joined(separator: " "))]"
    }

    func emit(_ op: Op) {
        code.append(op)
    }
    
    func eval(fromPc: Pc) async throws {
        try! await currentTask!.eval(fromPc: fromPc)
    }

    func pop() async -> Val? {
        await currentTask!.pop()
    }

    func push(_ val: Val) async {
        await currentTask!.push(val)
    }

    func startTask(pc: Pc = 0) {
        switchTask(Task(self, pc: pc))
    }

    func switchTask(_ task: Task) {
        if let t = currentTask {
            tasks.append(t)
        }

        currentTask = task
    }

    func switchTask() {
        switchTask(tasks.removeFirst())
    }
}

/*
 Now we're ready to take it for a spin.

 We'll settle for some simple arithmetics this time around, just to get an idea how everything works.
 */

let m = M()

let stringType = ValType("String")

let sayFun = Fun("say") {(m: M, pc: Pc) async throws -> Pc in
    let what = await m.pop()!
    println(what)
    return pc + 1
}

await m.emit(.push(Val(stringType, "hello task")))
await m.emit(.call(sayFun))
await m.emit(.yield())
await m.emit(.stop)

await m.startTask()
await m.startTask()
try await m.eval(fromPc: 0)

/*
 This prints [10], which is the final contents of the stack after adding 6 to 4.
 */

print(await m.dumpStack())
