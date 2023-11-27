struct Value: CustomStringConvertible {
    let data: Any
    let type: any ValueType

    var description: String { type.toString(self) }
    var toBool: Bool { type.toBool(self) }
    
    init<T>(_ type: BasicValueType<T>, _ data: T) {
        self.type = type
        self.data = data
    }

    func identifierEmit(_ vm: VM,
                        inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        try self.type.identifierEmit(self, vm, inNamespace: ns, withArguments: &args)
    }
}

protocol ValueType: CustomStringConvertible {
    associatedtype V
    
    var name: String { get }

    func cast(_ value: Value) throws -> V

    func equals(_ other: any ValueType) -> Bool
    
    func identifierEmit(_ value: Value, _ vm: VM, inNamespace: Namespace, withArguments args: inout [Form]) throws

    func toBool(_ value: Value) -> Bool
    func toString(_ value: Value) -> String
}

class BasicValueType<V>: ValueType {
    let name: String
    var description: String { name }

    init(_ name: String) {
        self.name = name
    }

    func cast(_ value: Value) throws -> V {
        let v = value as? V

        if v == nil {
            throw EvaluateError.typeMismatch(self, value.type)
        }

        return v!
    }

    func equals(_ other: any ValueType) -> Bool {
        if let rhs = other as? BasicValueType<V> {
            return self === rhs
        }

        return false
    }

    func identifierEmit(_ value: Value, _ vm: VM, inNamespace: Namespace, withArguments args: inout [Form]) throws {
        args.insert(Literal(value), at: 0)
    }

    func toString(_ value: Value) -> String {
        "\(value.data)"        
    }

    func toBool(_ value: Value) -> Bool {
        true
    }
}

enum EmitError: Error {
    case invalidSyntax
    case missingArgument
    case unknownIdentifier(String)
}

enum EvaluateError: Error {
    case missingValue
    case typeMismatch(any ValueType, any ValueType)
}

typealias PC = Int

struct Function: CustomStringConvertible {
    struct Call {
        let returnPc: PC
        let stackOffset: Int
        let target: Function
        
        init(_ target: Function, stackOffset: Int, returnPc: PC) {
            self.target = target
            self.stackOffset = stackOffset
            self.returnPc = returnPc
        }
    }

    typealias Body = (Function, VM) throws -> Void
    
    let arguments: [(String, any ValueType)]
    let body: Body
    let name: String

    var description: String { name }
    
    init(_ name: String, _ arguments: [(String, any ValueType)], _ body: @escaping Body) {
        self.name = name
        self.arguments = arguments
        self.body = body
    }

    func call(_ vm: VM) throws {
        if vm.stack.count < arguments.count {
            throw EvaluateError.missingValue
        }

        for i in 0..<arguments.count {
            let expected = arguments[i].1
            let actual  = vm.stack[vm.stack.count - arguments.count + i].type

            if !actual.equals(expected) {
                throw EvaluateError.typeMismatch(expected, actual)
            }
        }
        
        try body(self, vm)
    }
}

struct Macro: CustomStringConvertible {
    typealias Body = (Macro, VM, Namespace, inout [Form]) throws -> Void

    let arity: Int
    let body: Body
    let name: String

    var description: String { name }
    
    init(_ name: String, _ arity: Int, _ body: @escaping Body) {
        self.name = name
        self.arity = arity
        self.body = body
    }

    func emit(_ vm: VM, inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        if args.count < arity {
            throw EmitError.missingArgument
        }
        
        try body(self, vm, ns, &args)
    }
}

class Namespace {
    let parent: Namespace?
    var bindings: [String:Value] = [:]
    
    subscript(key: String) -> Value? {
        get {
            if let value = bindings[key] {
                value
            } else if parent != nil {
                parent![key]
            } else {
                nil
            }
        }
        set(value) {
            bindings[key] = value
        }
    }

    init(_ parent: Namespace? = nil) {
        self.parent = parent
    }
}

protocol Form: CustomStringConvertible {
    func cast<T: Form>(_ type: T.Type) throws -> T
    func emit(_ vm: VM, inNamespace: Namespace, withArguments: inout [Form]) throws
}

class BasicForm {    
    var description: String { "\(self)" }

    func cast<T: Form>(_ type: T.Type) throws -> T {
        let f = self as? T

        if f == nil {
            throw EmitError.invalidSyntax
        }

        return f!
    }
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

class Pair: BasicForm, Form {
    let left: Form
    let right: Form

    override var description: String { "\(left):\(right)" }

    init(_ left: Form, _ right: Form) {
        self.left = left
        self.right = right
    }

    func emit(_ vm: VM, inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        if left is Literal && right is Literal {
            args.insert(Literal(Value(std.pairType, ((left as! Literal).value,
                                                     (right as! Literal).value))),
                        at: 0)
        } else {
            try left.emit(vm, inNamespace: ns, withArguments: &args)
            try right.emit(vm, inNamespace: ns, withArguments: &args)
            vm.emit(.makePair)
        }
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

typealias Stack = [Value]

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

class VM {    
    enum Operation {
        case argument(Int)
        case call(Function)
        case goto(PC)
        case makePair
        case nop
        case or(PC)
        case popCall
        case push(Value)
        case stop
        case trace
    }

    var callStack: [Function.Call] {
        get {currentTask!.callStack}
        set(v) {currentTask!.callStack = v} 
    }
    
    var code: [Operation] = []
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
    
    @discardableResult
    func emit(_ op: Operation) -> PC {
        if trace { code.append(.trace) }
        let pc = code.count
        code.append(op)
        return pc
    }
    
    func evaluate(fromPc: PC) throws {
        pc = fromPc
        
        loop: while true {
            let op = code[pc]
 
            switch op {
            case let.argument(index):
                push(stack[callStack.last!.stackOffset+index])
                pc += 1
            case let .call(target):
                pc += 1
                try target.call(self)
            case let .goto(targetPc):
                pc = targetPc
            case .makePair:
                let right = pop()
                let left =  pop()
                push(Value(std.pairType, (left, right)))
                pc += 1
            case .nop:
                pc += 1
            case let .or(endPc):
                if pop().toBool {
                    pc = endPc
                } else {
                    pc += 1
                }
            case .popCall:
                let c = callStack.removeLast()
                stack.removeSubrange(c.stackOffset..<c.stackOffset+c.target.arguments.count)
                pc = c.returnPc
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
        stack.removeLast()
    }

    func push(_ value: Value) {
        stack.append(value)
    }

    func startTask(pc: PC = 0) {
        let t = Task(id: nextTaskId, startPc: pc)
        tasks.append(t)
        nextTaskId += 1
    }
}

class StandardLibrary: Namespace {
    class ArgumentType: BasicValueType<Int> {
        init() {
            super.init("Argument")
        }
        
        override func identifierEmit(_ value: Value, _ vm: VM,
                                     inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
            vm.emit(.argument(value.data as! Int))
        }
    }

    class FunctionType: BasicValueType<Function> {
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

    class IntType: BasicValueType<Int> {
        init() {
            super.init("Int")
        }
        
        override func toBool(_ value: Value) -> Bool {
            (value.data as! Int) != 0
        }
    }

    class MacroType: BasicValueType<Macro> {
        init() {
            super.init("Macro")
        }
        
        override func identifierEmit(_ value: Value, _ vm: VM,
                                     inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
            try (value.data as! Macro).emit(vm, inNamespace: ns, withArguments: &args)
        }
    }

    class PairType: BasicValueType<(Value, Value)> {
        init() {
            super.init("Pair")
        }
        
        override func toBool(_ value: Value) -> Bool {
            let v = value.data as! (Value, Value)
            return v.0.toBool && v.1.toBool
        }
    }

    class StringType: BasicValueType<String> {
        init() {
            super.init("String")
        }
        
        override func toBool(_ value: Value) -> Bool {
            (value.data as! String).count != 0
        }

        override func toString(_ value: Value) -> String {
            "\"\(value.data as! String)\""
        }        
    }

    let argumentType = ArgumentType()
    let functionType = FunctionType()
    let intType = IntType()
    let macroType = MacroType()
    let metaType = BasicValueType<any ValueType>("Meta")
    let pairType = PairType()
    let stringType = StringType()

    init() {
        super.init()
    
        self["Function"] = Value(metaType, functionType)
        self["Int"] = Value(metaType, intType)
        self["Macro"] = Value(metaType, macroType)
        self["Meta"] = Value(metaType, metaType)
        self["Pair"] = Value(metaType, pairType)
        self["String"] = Value(metaType, stringType)

        bindMacro("function", 3) {(_, vm, ns, args) throws in
            let id = try args.removeFirst().cast(Identifier.self).name

            let fargs = try args.removeFirst().cast(List.self).items.map {(it) in
                let p = try it.cast(Pair.self)
                let n = try p.left.cast(Identifier.self).name
                let tid = try p.right.cast(Identifier.self).name
                let t = ns[tid]

                if t == nil {
                    throw EmitError.unknownIdentifier(tid)
                }
                
                return (n, t!.data as! any ValueType)
            }
            
            let body = args.removeFirst()
            let skip = vm.emit(.nop)
            let startPc = vm.emitPc
            
            let f = Function(id, fargs) {(f, vm) throws in
                vm.callStack.append(Function.Call(f, stackOffset: vm.stack.count-fargs.count, returnPc: vm.pc))
                vm.pc = startPc
            }
        
            ns[id] = Value(self.functionType, f)
            let fns = Namespace(ns)
            
            for i in 0..<fargs.count {
                fns[fargs[i].0] = Value(self.argumentType, i)
            }
            
            try body.emit(vm, inNamespace: fns, withArguments: &args)
            vm.emit(.popCall)
            vm.code[skip] = .goto(vm.emitPc)
        }
    
        bindMacro("or", 2) {(_, vm, ns, args) throws in
            try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
            let or = vm.emit(.nop)
            try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
            vm.code[or] = .or(vm.emitPc)
        }
    
        bindMacro("trace", 0) {(_, vm, ns, args) throws in
            vm.trace = !vm.trace
        }
    
        bindFunction("+", [("left", intType), ("right", intType)]) {(_, vm) throws in
            let r = vm.pop()
            let l = vm.pop()
            vm.push(Value(self.intType, (l.data as! Int) + (r.data as! Int)))
        }
        
        bindFunction("yield", []) {(_, vm) throws in
            vm.tasks.append(vm.tasks.removeFirst())
        }
    }

    func bindFunction(_ name: String, _ args: [(String, any ValueType)], _ body: @escaping Function.Body) {
        self[name] = Value(functionType, Function(name, args, body))
    }

    func bindMacro(_ name: String, _ arity: Int, _ body: @escaping Macro.Body) {
        self[name] = Value(macroType, Macro(name, arity, body))
    }
}

let std = StandardLibrary()

/*
 Now we're ready to take it for a spin.

 We'll generate a user defined function called 'identity' that simply returns its argument.
 Right after the call we'll push the string "Returned" to verify that we end up in the right place.
 */

let vm = VM()

let functionForms: [Form] = [Identifier("function"),
                             Identifier("identity"),
                             List([Pair(Identifier("value"), Identifier("Int"))]),
                             Identifier("value")]

try functionForms.emit(vm, inNamespace: std)
let callForms: [Form] = [Identifier("identity"), Literal(Value(std.intType, 42))]
try callForms.emit(vm, inNamespace: std)

vm.emit(.push(Value(std.stringType, "Returned")))
vm.emit(.stop)
try vm.evaluate(fromPc: 0)

/*
 This prints [42, "Returned"].
 */

print(vm.stack)
