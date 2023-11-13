struct Value: CustomStringConvertible {
    let type: ValueType
    let data: Any

    var description: String { type.toString(self) }
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

class ValueType: CustomStringConvertible {
    let name: String
    var description: String { name }
    
    init(_ name: String) {
        self.name = name
    }

    func toString(_ value: Value) -> String {
        "\(value.data)"        
    }

    func identifierEmit(_ value: Value, _ vm: VM, inNamespace: Namespace, withArguments args: inout [Form]) throws {
        args.insert(Literal(value), at: 0)
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

enum EvalError: Error {
    case missingValue
    case typeMismatch(ValueType, ValueType)
}

typealias PC = Int

struct Function: CustomStringConvertible {
    /*
     Calls are used to construct the call stack for user defined functions.
     They keep track of the target being called, it's arguments and the return PC.
     */
    
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
    
    let arguments: [(String, ValueType)]
    let body: Body
    let name: String

    var description: String { name }
    
    init(_ name: String, _ arguments: [(String, ValueType)], _ body: @escaping Body) {
        self.name = name
        self.arguments = arguments
        self.body = body
    }

    func call(_ vm: VM) throws {
        if vm.stack.count < arguments.count {
            throw EvalError.missingValue
        }

        for i in 0..<arguments.count {
            let expected = arguments[i].1
            let actual  = vm.stack[vm.stack.count - arguments.count + i].type

            if actual !== expected {
                throw EvalError.typeMismatch(expected, actual)
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
    func emit(_ vm: VM, inNamespace: Namespace, withArguments: inout [Form]) throws
    func cast<T: Form>(_ type: T.Type) throws -> T
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
        args.insert(Literal(Value(std.pairType, (left, right))), at: 0)
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

class StandardLibrary: Namespace {
    class ArgumentType: ValueType {
        init() {
            super.init("Argument")
        }
        
        override func identifierEmit(_ value: Value, _ vm: VM,
                                     inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
            vm.emit(.argument(value.data as! Int))
        }
    }

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

    class IntType: ValueType {
        init() {
            super.init("Int")
        }
        
        override func toBool(_ value: Value) -> Bool {
            (value.data as! Int) != 0
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

    class PairType: ValueType {
        init() {
            super.init("Pair")
        }
        
        override func toBool(_ value: Value) -> Bool {
            let v = value.data as! (Value, Value)
            return v.0.toBool && v.1.toBool
        }
    }

    class StringType: ValueType {
        init() {
            super.init("String")
        }
        
        override func toBool(_ value: Value) -> Bool {
            (value.data as! String).count != 0
        }
    }

    let argumentType = ArgumentType()
    let functionType = FunctionType()
    let intType = IntType()
    let macroType = MacroType()
    let metaType = ValueType("Meta")
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
                
                return (n, t!.data as! ValueType)
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
            vm.emit(.popCall(f))
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

    func bindFunction(_ name: String, _ args: [(String, ValueType)], _ body: @escaping Function.Body) {
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
try vm.eval(fromPc: 0)

/*
 This prints [42, "Returned"].
 */

print(vm.stack)
