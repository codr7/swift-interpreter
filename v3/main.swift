/* Version 3 */

/*
 We use structs to represent values, every value has a type.
 Types allow specializing behaviour for differend kinds of values.
 */

class ValueType {
    let name: String
    
    init(_ name: String) {
        self.name = name
    }

    func dump(_ value: Value) -> String {
        "\(value.data)"        
    }

    func identifierEmit(_ value: Value, _ vm: VM, inNamespace: Namespace, withArguments: inout [Form]) throws {
        vm.emit(.push(value))
    }

    func toBool(_ value: Value) -> Bool {
        true
    }
}

struct Value: CustomStringConvertible {
    let type: ValueType
    let data: Any

    var description: String { type.dump(self) }
    var toBool: Bool { type.toBool(self) }
    
    init(_ type: ValueType, _ data: Any) {
        self.type = type
        self.data = data
    }

    func identifierEmit(_ vm: VM, inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        try self.type.identifierEmit(self, vm, inNamespace: ns, withArguments: &args)
    }
}

public enum EmitError: Error {
    case unknownIdentifier(String)
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

struct Fun: CustomStringConvertible {
    typealias Body = (VM, inout PC) throws -> Void
    
    let name: String
    let body: Body
    var description: String { "Fun(name)" }
    
    init(_ name: String, _ body: @escaping Body) {
        self.name = name
        self.body = body
    }

    func call(_ vm: VM, pc: inout PC) throws {
        try body(vm, &pc)
    }
}

/*
 Macros are named constructs that emit operations on use.
 
 Since they are free to evaluate their arguments and manipulate their environment any way they choose,
 they may be used to add new features to the language, control structures etc.
 */

struct Macro: CustomStringConvertible {
    typealias Body = (VM, Namespace, inout [Form]) throws -> Void
    
    let name: String
    let body: Body

    var description: String {"Macro(name)"}
    
    init(_ name: String, _ body: @escaping Body) {
        self.name = name
        self.body = body
    }

    func emit(_ vm: VM, inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        try body(vm, ns, &args)
    }
}

class Namespace {
    let parent: Namespace?
    var bindings: [String:Value] = [:]
    
    subscript(key: String) -> Value? {
        get {
            return if let value = bindings[key] {
                value
            } else if parent != nil {
                parent![key]
            } else {
                nil
            }
        }
        set(value) { bindings[key] = value }
    }

    init(_ parent: Namespace? = nil) {
        self.parent = parent
    }
}

/*
 Forms are the bits and pieces that the syntax consists of, the initial step in converting code to bytecode
 operations.
 */

protocol Form: CustomStringConvertible {
    func emit(_ vm: VM, inNamespace: Namespace, withArguments: inout [Form]) throws
}

class BasicForm {    
    var description: String { "\(self)" }
}

class Identifier: BasicForm, Form {    
    let name: String
    override var description: String { name }
    
    init(_ name: String) {
        self.name = name
    }

    func emit(_ vm: VM, inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        if let value = ns[name] {
            try value.identifierEmit(vm, inNamespace: ns, withArguments: &args)
        } else {
            throw EmitError.unknownIdentifier(name)
        }
    }
}

class Literal: BasicForm, Form {
    let value: Value
    override var description: String { "\(value)" }

    init(_ value: Value) {
        self.value = value
    }

    func emit(_ vm: VM, inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        vm.emit(.push(value))
    }
}

extension [Form] {
    func emit(_ vm: VM, inNamespace ns: Namespace) throws {
        var fs = self
        
        while fs.count > 0 {
            try fs.removeFirst().emit(vm, inNamespace: ns, withArguments: &fs)
        }
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
    case nop
    case or(PC)
    case push(Value)
    case stop
    case trace
    case yield
}

/* Tasks represent independent flows of execution with separate stacks and program counters. */

class Task {
    typealias Id = Int

    let id: Id
    var stack: [Value] = []
    var pc: PC

    init(id: Id, startPc: PC) {
        self.id = id
        self.pc = startPc
    }
}

class MacroType: ValueType {
    init() {
        super.init("Macro")
    }

    override func identifierEmit(_ value: Value, _ vm: VM,
                        inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        try (value.data as! Macro).emit(vm, inNamespace: ns, withArguments: &args)
    }
}

let macroType = MacroType()

/*
 The virtual machine is where the rubber finally meets the road.
 */

class VM {    
    var code: [Op] = []
    var currentTask: Task? {tasks[0]}
    var emitPc: PC {code.count}
    var nextTaskId = 0

    var pc: PC {
        get {currentTask!.pc}
        set(pc) {currentTask!.pc = pc}
    }

    var tasks: [Task] = []
    var trace = false
    
    init() {
        startTask()
    }
    
    func dumpStack() -> String {
        "[\(currentTask!.stack.map({"\($0)"}).joined(separator: " "))]"
    }

    @discardableResult
    func emit(_ op: Op) -> PC {
        if trace { code.append(.trace) }
        let pc = code.count
        code.append(op)
        return pc
    }
    
    func eval(fromPc: PC) throws {
        pc = fromPc
        
        loop: while true {
            let op = code[pc]
 
            switch op {
            case let .call(target):
                try target.call(self, pc: &pc)
            case .nop:
                pc += 1
            case let .or(endPc):
                let l = peek()

                if l!.toBool {
                    pc = endPc
                } else {
                    _ = pop()
                    pc += 1
                }
            case let .push(value):
                push(value)
                pc += 1
            case .stop:
                break loop
            case .trace:
                pc += 1
                print("\(pc) \(code[pc])")
            case .yield:
                pc += 1
                switchTask()
                try eval(fromPc: vm.currentTask!.pc)
            }
        }
    }

    func peek() -> Value? {
        currentTask!.stack.last
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

    func switchTask() {
        tasks.append(tasks.removeFirst())
    }
}

/*
 Now we're ready to take it for a spin.

 We'll create forms representing the following expression and emit the corresponding operations:
 0 or 42
 */

let vm = VM()

class IntType: ValueType {
    init() {
        super.init("Int")
    }

    override func toBool(_ value: Value) -> Bool {
        (value.data as! Int) != 0
    }
}

let intType = IntType()

let orMacro = Macro("or") {(vm: VM, ns: Namespace, args: inout [Form]) throws in
    let orPc = vm.emit(.nop)
    try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
    vm.code[orPc] = .or(vm.emitPc)
}

let namespace = Namespace()
namespace["or"] = Value(macroType, orMacro)

let forms: [Form] = [Literal(Value(intType, 0)), Identifier("or"), Literal(Value(intType, 42))]
try forms.emit(vm, inNamespace: namespace)
vm.emit(.stop)

try vm.eval(fromPc: 0)

/*
 This prints [42], which is the result of '0 or 42'.
*/

print(vm.dumpStack())
