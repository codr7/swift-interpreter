/*
 Version 4
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

    func identifierEmit(_ vm: VM,
                        inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
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

enum EmitError: Error {
    case missingArgument
    case unknownIdentifier(String)
}

enum EvalError: Error {
    case missingValue
}

typealias PC = Int

/*
 Functions can be either primitive or user defined.

 Primitive functions are implemented in Swift inside the body, while user defined functions are implemented as virtual operations and use the call stack.
 */

struct Function: CustomStringConvertible {
    /*
     Calls are used to construct the call stack for user defined functions.
     They keep track of the target being called, it's arguments and the return PC.
     */
    
    struct Call {
        let target: Function
        let stackOffset: Int
        let returnPc: PC
        
        init(_ target: Function, _ stackOffset: Int, _ returnPc: PC) {
            self.target = target
            self.stackOffset = stackOffset
            self.returnPc = returnPc
        }
    }

    typealias Body = (Function, VM) throws -> Void
    
    let name: String
    let arguments: [String]
    let body: Body
    var description: String { "Function(name)" }
    
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

    var description: String {"Macro(name)"}
    
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

class List: BasicForm, Form {
    let items: [Form]
    override var description: String { "(\(items.map({"\($0)"}).joined(separator: " "))" }

    init(_ items: [Form]) {
        self.items = items
    }

    func emit(_ vm: VM, inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        try items.emit(vm, inNamespace: ns)
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
    case argument(Int)
    case call(Function)
    case goto(PC)
    case nop
    case or(PC)
    case popCall(Function)
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

    var callStack: [Function.Call] = []
    var pc: PC
    var stack: Stack = []

    init(id: Id, startPc: PC) {
        self.id = id
        self.pc = startPc
    }
}

/*
 The virtual machine is where the rubber finally meets the road.
 */

class VM {    
    var callStack: [Function.Call] {
        get {currentTask!.callStack}
        set(v) {currentTask!.callStack = v} 
    }
    
    var code: [Op] = []
    var currentTask: Task? {tasks[0]}
    var emitPc: PC {code.count}
    var nextTaskId = 0

    var pc: PC {
        get {currentTask!.pc}
        set(pc) {currentTask!.pc = pc}
    }

    var stack: Stack {
        get {currentTask!.stack}
        set(v) {currentTask!.stack = v}
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
            case let.argument(index):
                vm.push(vm.stack[vm.callStack.last!.stackOffset+index])
                pc += 1
            case let .call(target):
                pc += 1
                try target.call(self)
            case let .goto(targetPc):
                pc = targetPc
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
            case let .popCall(target):
                let c = vm.callStack.removeLast()
                vm.stack.removeSubrange(c.stackOffset..<c.stackOffset+target.arguments.count)
                pc = c.returnPc
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

let stdLib = Namespace()

func stdFunction(_ name: String, _ args: [String], _ body: @escaping Function.Body) {
    stdLib[name] = Value(functionType, Function(name, args, body))
}

func stdMacro(_ name: String, _ body: @escaping Macro.Body) {
    stdLib[name] = Value(macroType, Macro(name, body))
}

class ArgumentType: ValueType {
    init() {
        super.init("Argument")
    }

    override func identifierEmit(_ value: Value, _ vm: VM,
                                 inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        vm.emit(.argument(value.data as! Int))
    }
}

let argumentType = ArgumentType()

class FunctionType: ValueType {
    init() {
        super.init("Function")
    }

    override func identifierEmit(_ value: Value, _ vm: VM,
                                 inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        let f = value.data as! Function
        
        for _ in 0..<f.arguments.count {
            if args.isEmpty {
                throw EmitError.missingArgument
            }
            
            try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
        }

        vm.emit(.call(f))
    }
}

let functionType = FunctionType()
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

class StringType: ValueType {
    init() {
        super.init("String")
    }

    override func toBool(_ value: Value) -> Bool {
        (value.data as! String).count != 0
    }
}

let stringType = StringType()
stdLib["String"] = Value(metaType, stringType)

let functionMacro = Macro("function") {(_, vm, ns, args) throws in
    let id = (args.removeFirst() as! Identifier).name
    let fargs = (args.removeFirst() as! List).items.map {($0 as! Identifier).name}
    let body = args.removeFirst()
    let skip = vm.emit(.nop)
    let startPc = vm.emitPc

    let f = Function(id, fargs) {(f, vm) throws in
        vm.callStack.append(Function.Call(f, vm.stack.count-fargs.count, vm.pc))
        vm.pc = startPc
    }

    ns[id] = Value(functionType, f)
    let fns = Namespace(ns)

    for i in 0..<fargs.count {
        fns[fargs[i]] = Value(argumentType, i)
    }

    try body.emit(vm, inNamespace: fns, withArguments: &args)
    vm.emit(.popCall(f))
    vm.code[skip] = .goto(vm.emitPc)
}

stdLib["function"] = Value(macroType, functionMacro)

stdMacro("or") {(_, vm, ns, args) throws in
    try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
    let or = vm.emit(.nop)
    try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
    vm.code[or] = .or(vm.emitPc)
}

stdFunction("+", ["left", "right"]) {(_, vm) throws in
    let r = vm.pop()!
    let l = vm.pop()!
    vm.push(Value(intType, (l.data as! Int) + (r.data as! Int)))
}

stdFunction("yield", []) {(_, vm) throws in
    vm.tasks.append(vm.tasks.removeFirst())
}

/*
 Now we're ready to take it for a spin.

 We'll generate a user defined function called 'identity' that simply returns its argument.
 Right after the call we'll push the string "Returned" to verify that we end up in the right place.
 */

let vm = VM()

let functionForms: [Form] = [Identifier("function"),
                             Identifier("identity"),
                             List([Identifier("value")]),
                             Identifier("value")]

try functionForms.emit(vm, inNamespace: stdLib)
let callForms: [Form] = [Identifier("identity"), Literal(Value(intType, 42))]
try callForms.emit(vm, inNamespace: stdLib)

vm.emit(.push(Value(stringType, "Returned")))
vm.emit(.stop)
try vm.eval(fromPc: 0)

/*
 This prints [42 "Returned"].
 */

print(vm.dumpStack())
