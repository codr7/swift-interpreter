/***
 The goal here is to provide the foundation for an interpreter in Swift that is simple enough to understand,
 yet substantial enough to continue building upon and playing around with.

 A taste of interpreters, Swift, or both; depending on where you're coming from.

 Rather than starting with syntax and parsers, we'll work from the bottom up.
 
 To try it out; simply download and install Swift, and run `swift main.swift`.
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

typealias PC = Int

/*
 Functions take the current program counter as a reference argument when called,
 which allows them to decide where to return to.

 This time around; we'll only deal with primitives, which typically simply increase the program counter.
 
 But once we get to user defined functions, this allows us to jump to the actual code of the called function.
 */

class Fun {
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
    case push(V)
    case stop
}

/*
 The virtual machine is where the rubber finally meets the road.
 */

class VM {
    var code: [Op] = []
    var stack: [V] = []

    func dumpStack() -> String {
        "[\(stack.map({$0.dump()}).joined(separator: " "))]"
    }

    func emit(_ op: Op) {
        code.append(op)
    }
    
    func eval(fromPc: PC) throws {
        var pc = fromPc
        
        loop: while true {
            let op = code[pc]
 
            switch op {
            case let .call(target):
                try target.call(self, pc: &pc)
            case let .push(v):
                stack.append(v)
                pc += 1
                break
            case .stop:
                break loop
            }
        }
    }

    func pop() -> V? {
        stack.removeLast()
    }

    func push(_ v: V) {
        stack.append(v)
    }
}

/*
 Now we're ready to take it for a spin.

 We'll settle for some simple arithmetics this time around, just to get an idea how everything works.
 */

let vm = VM()

let intType = T("Int")

let addFun = Fun("+") {(vm: VM, pc: inout PC) throws -> Void in
    let r = vm.pop()!
    let l = vm.pop()!
    vm.push(V(intType, (l.data as! Int) + (r.data as! Int)))
    pc += 1
}

vm.emit(.push(V(intType, 6)))
vm.emit(.push(V(intType, 4)))
vm.emit(.call(addFun))
vm.emit(.stop)

try vm.eval(fromPc: 0)

/*
 This prints [10], which is the final contents of the stack after adding 6 to 4.
 */

print(vm.dumpStack())
