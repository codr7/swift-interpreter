/*
 Version 3
 */

/*
 We'll use structs to represent values, every value has a type.
 */

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

/*
 Types allow specializing behaviour for differend kinds of values.
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

    func identifierEmit(_ value: Value, _ vm: VM, inNamespace: Namespace, withArguments: inout [Form]) throws {
        vm.emit(.push(value))
    }

    func toBool(_ value: Value) -> Bool {
        true
    }
}

/*
 Errors are defined by enums separated by category.
 */

public enum EmitError: Error {
    case unknownIdentifier(String)
}

public enum EvalError: Error {
    case missingValue
}

/*
 Functions can be either primitive or user defined.

 Primitive functions are implemented in Swift inside the body, while user defined functions are implemented as virtual operations and use the call stack.

 We'll only deal with primitives for now.
 */

struct Function: CustomStringConvertible {
    typealias Body = (Function, VM) throws -> Void
    
    let name: String
    let arguments: [String]
    let body: Body

    var description: String { name }
    
    init(_ name: String, _ arguments: [String], _ body: @escaping Body) {
        self.name = name
        self.arguments = arguments
        self.body = body
    }

    func call(_ vm: VM) throws {
        if vm.stack.count < arguments.count {
            throw EvalError.missingValue
        }
        
        try body(self, vm)
    }
}

/*
 Macros are named constructs that emit operations on use.
 
 Since they are free to evaluate their arguments and manipulate their environment any way they choose,
 they may be used to add new features to the language, control structures etc.
 */

struct Macro: CustomStringConvertible {
    typealias Body = (Macro, VM, Namespace, inout [Form]) throws -> Void
    
    let name: String
    let body: Body

    var description: String { name }
    
    init(_ name: String, _ body: @escaping Body) {
        self.name = name
        self.body = body
    }

    func emit(_ vm: VM, inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        try body(self, vm, ns, &args)
    }
}

/*
 Namespaces map symbols to values, and are used for looking up identifiers when emitting forms.
 */

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
 Forms are the bits and pieces that the syntax consists of, the initial step in converting code to 
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

typealias PC = Int

/*
 Operations are the things that our virtual machine executes.

 Any kind of code we want to run on it, regardless of syntax; needs to be reduced to a sequence of operations.

 The reason there's a separate case for stopping is to avoid having to check in the eval loop,
 which needs to be as fast as possible.
 */

enum Op {
    case call(Function)
    case nop
    case or(PC)
    case push(Value)
    case stop
    case trace
}

typealias Stack = [Value]

/*
 Tasks represent independent flows of execution with separate stacks and program counters.
 */

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
    var code: [Op] = []
    var currentTask: Task? {tasks[0]}
    var emitPc: PC {code.count}
    var nextTaskId = 0

    var pc: PC {
        get {currentTask!.pc}
        set(pc) {currentTask!.pc = pc}
    }

    var stack: Stack {currentTask!.stack}
    var tasks: [Task] = []
    var trace = false
    
    init() {
        startTask()
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
                pc += 1
                try target.call(self)
            case .nop:
                pc += 1
            case let .or(endPc):
                if let l = peek() {
                    if l.toBool {
                        pc = endPc
                    } else {
                        _ = pop()
                        pc += 1
                    }
                } else {
                    throw EvalError.missingValue
                }
            case let .push(value):
                push(value)
                pc += 1
            case .stop:
                break loop
            case .trace:
                pc += 1
                print("\(pc) \(code[pc])")
            }
        }
    }

    func peek() -> Value? {
        currentTask!.stack.last
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

/*
 The humble beginnings of a standard library.
 */

let stdLib = Namespace()

func stdFunction(_ name: String, _ args: [String], _ body: @escaping Function.Body) {
    stdLib[name] = Value(functionType, Function(name, args, body))
}

func stdMacro(_ name: String, _ body: @escaping Macro.Body) {
    stdLib[name] = Value(macroType, Macro(name, body))
}

let functionType = ValueType("Function")
stdLib["Function"] = Value(metaType, functionType)

class IntType: ValueType {
    init() {
        super.init("Int")
    }

    override func toBool(_ value: Value) -> Bool {
        (value.data as! Int) != 0
    }
}

let intType = IntType()
stdLib["Int"] = Value(metaType, intType)

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
stdLib["Macro"] = Value(metaType, macroType)

let metaType = ValueType("Meta")
stdLib["Meta"] = Value(metaType, metaType)

stdMacro("or") {(_, vm, ns, args) throws in
    try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
    let orPc = vm.emit(.nop)
    try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
    vm.code[orPc] = .or(vm.emitPc)
}

stdMacro("trace") {(_, vm, ns, args) throws in
    vm.trace = !vm.trace
}

stdFunction("+", ["left", "right"]) {(_, vm) throws in
    let r = vm.pop()
    let l = vm.pop()
    vm.push(Value(intType, (l.data as! Int) + (r.data as! Int)))
}

stdFunction("yield", []) {(_, vm) throws in
    vm.tasks.append(vm.tasks.removeFirst())
}

/*
 Now we're ready to take it for a spin.

 We'll create forms representing the following expression and emit the corresponding operations:
 or 1 3
 */

let vm = VM()
let forms: [Form] = [Identifier("or"), Literal(Value(intType, 1)), Literal(Value(intType, 3))]
try forms.emit(vm, inNamespace: stdLib)
vm.emit(.stop)
try vm.eval(fromPc: 0)

/*
 This prints 1, since `or` short-circuits.
 */

print(vm.pop())
