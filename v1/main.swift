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
 Functions take the current program counter as a reference argument,
 which allows them to decide where to return to.

 This time around, we'll only deal with primitives, that have no virtual code;
 which means that they'll typically simply increase the program counter.
 
 But once we get to user defined functions, this allows us to jump to the actual code of the called function.
 */

class Fun {
    typealias Body = (M, inout Pc) throws -> Void
    
    let name: String
    let body: Body

    init(_ name: String, _ body: @escaping Body) {
        self.name = name
        self.body = body
    }

    func call(_ m: M, returnPc: inout Pc) throws {
        try body(m, &returnPc)
    }
}

/*
 Operations are the things that our virtual machine executes.

 Any kind of code we want to run on it, regardless of syntax; needs to be reduced to a sequence of operations.

 The reason there's a separate case for stopping execution is to avoid having to check in the eval loop,
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

class M {
    var code: [Op] = []
    var stack: [Val] = []

    func dumpStack() -> String {
        "[\(stack.map({$0.dump()}).joined(separator: " "))]"
    }

    func emit(_ op: Op) {
        code.append(op)
    }
    
    func eval(fromPc: Pc) throws {
        var pc = fromPc
        
        loop: while true {
            let op = code[pc]
 
            switch op {
            case let .call(target):
                try target.call(self, returnPc: &pc)
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
        return stack.removeLast()
    }

    func push(_ val: Val) {
        stack.append(val)
    }
}

/*
 Now we're ready to take it for a spin.

 We'll settle for some simple arithmetics this time around, just to get an idea how everything works.
 */

let m = M()

let intType = ValType("Int")

let addFun = Fun("+") {(m: M, returnPc: inout Pc) throws -> Void in
    let r = m.pop()!
    let l = m.pop()!
    m.push(Val(intType, (l.data as! Int) + (r.data as! Int)))
    returnPc += 1
}

m.emit(.push(Val(intType, 6)))
m.emit(.push(Val(intType, 4)))
m.emit(.call(addFun))
m.emit(.stop)

try m.eval(fromPc: 0)

/*
 This prints [10], which is the final contents of the stack after adding 6 to 4.
 */

print(m.dumpStack())
