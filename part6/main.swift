import Foundation

struct Value: CustomStringConvertible {
    let data: Any
    let type: any ValueType

    var description: String { type.toString(self) }
    var toBool: Bool { type.toBool(self) }

    init(_ type: any ValueType, _ data: Any) {
        self.type = type
        self.data = data
    }

    func equals(_ other: Value) -> Bool {
        type.equals(self, other)
    }

    func identifierEmit(_ vm: VM, at pos: Position,
                        inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        try self.type.identifierEmit(self, vm, at: pos, inNamespace: ns, withArguments: &args)
    }
}

protocol ValueType: CustomStringConvertible {
    associatedtype V
    
    var name: String { get }

    func cast(_ value: Value) -> V
    func equals(_ other: any ValueType) -> Bool
    func equals(_ value1: Value, _ value2: Value) -> Bool
    
    func identifierEmit(_ value: Value,
                        _ vm: VM,
                        at: Position,
                        inNamespace: Namespace,
                        withArguments args: inout [Form]) throws

    func safeCast(_ value: Value, at: Position) throws -> V
    func toBool(_ value: Value) -> Bool
    func toString(_ value: Value) -> String
}

class BasicType<V> {
    let name: String
    var description: String { name }

    init(_ name: String) {
        self.name = name
    }

    func cast(_ value: Value) -> V {
        value.data as! V
    }

    func equals(_ other: any ValueType) -> Bool {
        if let rhs = other as? BasicType<V> {
            return self === rhs
        }

        return false
    }

    func identifierEmit(_ value: Value, _ vm: VM, at pos: Position, inNamespace: Namespace, withArguments args: inout [Form]) throws {
        args.insert(Literal(pos, value), at: 0)
    }

    func safeCast(_ value: Value, at pos: Position) throws -> V {
        let v = value.data as? V
        if v == nil { throw EvaluateError.typeMismatch(pos) }
        return v!
    }

    func toBool(_ value: Value) -> Bool {
        true
    }

    func toString(_ value: Value) -> String {
        "\(value.data)"        
    }
}

enum EmitError: Error {
    case invalidSyntax(Position)
    case missingArgument(Position)
    case unknownIdentifier(Position, String)
}

enum EvaluateError: Error {
    case arityMismatch(Position)
    case checkFailed(Position, Value, Value)
    case missingValue(Position)
    case typeMismatch(Position)
}

enum ReadError: Error {
    case invalidSyntax(Position)
}

typealias PC = Int

class Function: CustomStringConvertible {
    struct Call {
        let returnPc: PC
        let position: Position
        let stackOffset: Int
        let target: Function
        
        init(_ target: Function, at pos: Position, stackOffset: Int, returnPc: PC) {
            self.target = target
            self.position = pos
            self.stackOffset = stackOffset
            self.returnPc = returnPc
        }
    }

    typealias Body = (Function, VM, Position) throws -> Void
    
    let arguments: [(String, any ValueType)]
    let body: Body
    let name: String

    var description: String { name }
    
    init(_ name: String, _ arguments: [(String, any ValueType)], _ body: @escaping Body) {
        self.name = name
        self.arguments = arguments
        self.body = body
    }

    func call(_ vm: VM, at pos: Position) throws {
        if vm.stack.count < arguments.count {
            throw EvaluateError.missingValue(pos)
        }

        for i in 0..<arguments.count {
            let expected = arguments[i].1
            let actual  = vm.stack[vm.stack.count - arguments.count + i].type

            if !actual.equals(expected) {
                throw EvaluateError.typeMismatch(pos)
            }
        }
        
        try body(self, vm, pos)
    }
}

class Macro: CustomStringConvertible {
    typealias Body = (Macro, VM, Position, Namespace, inout [Form]) throws -> Void
    
    let arity: Int
    let body: Body
    let name: String

    var description: String { name }
    
    init(_ name: String, _ arity: Int, _ body: @escaping Body) {
        self.name = name
        self.arity = arity
        self.body = body
    }

    func emit(_ vm: VM, at pos: Position, inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        if args.count < arity {
            throw EmitError.missingArgument(pos)
        }

        try body(self, vm, pos, ns, &args)
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
 Positions are used to track source code locations.
 */

struct Position: CustomStringConvertible {
    let source: String

    var column: Int
    var description: String { "\(source)@\(line):\(column)" }
    var line: Int

    init(_ source: String, line: Int = 1, column: Int = 0) {
        self.source = source
        self.line = line
        self.column = column
    }
}

protocol Form: CustomStringConvertible {
    var position: Position {get}
    func cast<T: Form>(_ type: T.Type) throws -> T
    func emit(_ vm: VM, inNamespace: Namespace, withArguments: inout [Form]) throws
}

class BasicForm {
    let position: Position
    var description: String { "\(self)" }

    init(_ position: Position) {
        self.position = position
    }

    func cast<T: Form>(_ type: T.Type) throws -> T {
        let f = self as? T

        if f == nil {
            throw EmitError.invalidSyntax(position)
        }

        return f!
    }
}

class ArrayForm: BasicForm, Form {
    let items: [Form]
    override var description: String { "[\(items.map({"\($0)"}).joined(separator: " "))]" }

    init(_ position: Position, _ items: [Form]) {
        self.items = items
        super.init(position)
    }

    func emit(_ vm: VM,
              inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        try items.emit(vm, inNamespace: ns)
        vm.emit(.makeArray(items.count))
    }
}

class Identifier: BasicForm, Form {    
    let name: String
    override var description: String { name }
    
    init(_ position: Position, _ name: String) {
        self.name = name
        super.init(position)
    }

    func emit(_ vm: VM,
              inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        if let value = ns[name] {
            try value.identifierEmit(vm, at: position, inNamespace: ns, withArguments: &args)
        } else {
            throw EmitError.unknownIdentifier(position, name)
        }
    }
}

class List: BasicForm, Form {
    let items: [Form]
    override var description: String { "(\(items.map({"\($0)"}).joined(separator: " "))" }

    init(_ position: Position, _ items: [Form]) {
        self.items = items
        super.init(position)
    }

    func emit(_ vm: VM,
              inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        try items.emit(vm, inNamespace: ns)
    }
}

class Literal: BasicForm, Form {
    let value: Value
    override var description: String { "\(value)" }

    init(_ position: Position, _ value: Value) {
        self.value = value
        super.init(position)
    }

    func emit(_ vm: VM,
              inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        vm.emit(.push(value))
    }
}

class Pair: BasicForm, Form {
    let left: Form
    let right: Form

    override var description: String { "\(left):\(right)" }

    init(_ pos: Position, _ left: Form, _ right: Form) {
        self.left = left
        self.right = right
        super.init(pos)
    }

    func emit(_ vm: VM, inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        if left is Literal && right is Literal {
            args.insert(Literal(position, Value(std.pairType, ((left as! Literal).value,
                                                               (right as! Literal).value))),
                        at: 0)
        } else {
            try left.emit(vm, inNamespace: ns, withArguments: &args)
            try right.emit(vm, inNamespace: ns, withArguments: &args)
            vm.emit(.makePair)
        }
    }
}

class Reference: BasicForm, Form {
    let id: String
    override var description: String { "&\(id)" }

    init(_ position: Position, _ id: String) {
        self.id = id
        super.init(position)
    }

    func emit(_ vm: VM,
              inNamespace ns: Namespace,
              withArguments args: inout [Form]) throws {
        let v = ns[id]

        if v == nil {
            throw EmitError.unknownIdentifier(position, id)
        }
        
        vm.emit(.push(v!))
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
        case branch(PC)
        case call(Position, Function)
        case check(Position)
        case goto(PC)
        case makeArray(Int)
        case makePair
        case nop
        case or(PC)
        case popCall
        case push(Value)
        case stop
        case task(PC)
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
            case let .branch(elsePc):
                if pop().toBool {
                    pc += 1
                } else {
                    pc = elsePc
                }
            case let .call(pos, target):
                pc += 1
                try target.call(self, at: pos)
            case let .check(pos):
                if stack.isEmpty { throw EvaluateError.missingValue(pos) }
                let expected = pop()
                try evaluate(fromPc: pc+1)
                if stack.isEmpty { throw EvaluateError.missingValue(pos) }
                let actual = pop()

                if !actual.equals(expected) {
                    throw EvaluateError.checkFailed(pos, expected, actual)
                }
            case let .goto(targetPc):
                pc = targetPc
            case let .makeArray(count):
                let items = stack.suffix(count)
                stack.removeLast(count)
                push(Value(std.arrayType, Array(items)))
                pc += 1
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
            case let .task(endPc):
                startTask(pc: pc+1)
                pc = endPc
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

/*
 Inputs are used to simplify reading input from strings.
 */

struct Input {
    var data: String

    init(_ data: String = "") {
        self.data = data
    }

    mutating func append(_ data: String) {
        self.data.append(data)
    }
    
    func peekChar() -> Character? {
        return data.first
    }
    
    mutating func popChar() -> Character? {
        data.isEmpty ? nil : data.removeFirst()
    }

    mutating func pushChar(_ char: Character) {
        data.insert(char, at: data.startIndex)
    }

    mutating func reset() {
        data = ""
    }
}

/*
 Readers convert source code to forms.
 */

typealias Reader = (_ input: inout Input, _ output: inout [Form], _ pos: inout Position) throws -> Bool

let readers = [readWhitespace,
               readDot,
               readReference,
               readPair,
               readList,
               readArray,
               readString,
               readInt,
               readIdentifier]

func readForm(_ input: inout Input, _ output: inout [Form], _ pos: inout Position) throws -> Bool {
    for r in readers {
        if try r(&input, &output, &pos) { return true }
    }

    return false
}

func readAll(_ reader: Reader, _ input: inout Input, _ output: [Form], _ pos: inout Position) throws -> [Form] {
    var result = output
    while try reader(&input, &result, &pos) {}
    return result
}

func readArray(_ input: inout Input, _ output: inout [Form], _ pos: inout Position) throws -> Bool {
    let fpos = pos
    var c = input.popChar()
    
    if c != "[" {
        if c != nil { input.pushChar(c!) }
        return false
    }
    
    pos.column += 1
    let items = try readAll(readForm, &input, [], &pos)
    c = input.popChar()

    if c != "]" {
        if c != nil { input.pushChar(c!) }
        throw ReadError.invalidSyntax(fpos)
    }
    
    pos.column += 1
    output.append(ArrayForm(fpos, items))
    return true
}

func readDot(_ input: inout Input, _ output: inout [Form], _ pos: inout Position) throws -> Bool {    
    let fpos = pos
    let c = input.popChar()
    
    if c != "." {
        if c != nil { input.pushChar(c!) }
        return false
    }
    
    pos.column += 1
    let argument = output.removeLast()

    if !(try readIdentifier(&input, &output, &pos)) {
        throw ReadError.invalidSyntax(fpos)
    }

    output.append(argument)
    return true
}

func readIdentifier(_ input: inout Input, _ output: inout [Form], _ pos: inout Position) throws -> Bool {
    let fpos = pos
    var name = ""
    
    while let c = input.popChar() {
        if c.isWhitespace || c == "(" || c == ")" || c == "[" || c == "]" || c == ":" || c == "&" || c == "." {
            input.pushChar(c)
            break
        }
        
        name.append(c)
        pos.column += 1
    }
    
    if (name.count == 0) { return false }
    output.append(Identifier(fpos, name))
    return true
}

func readInt(_ input: inout Input, _ output: inout [Form], _ pos: inout Position) throws -> Bool {
    let fpos = pos
    var v = 0
    var neg = false
    
    let c = input.popChar()
    if c == nil { return false }
    
    if c == "-" {
        if let c = input.popChar() {
            if c.isNumber {
                neg = true
                pos.column += 1
            } else {
                input.pushChar(c)
                input.pushChar("-")
            }
        }
    } else {
        input.pushChar(c!)
    }
    
    while let c = input.popChar() {
        if !c.isNumber {
            input.pushChar(c)
            break
        }
        
        v *= 10
        v += c.hexDigitValue!
        pos.column += 1
    }
    
    if (pos.column == fpos.column) { return false; }
    output.append(Literal(fpos, Value(std.intType, neg ? -v : v)))
    return true
}

func readList(_ input: inout Input, _ output: inout [Form], _ pos: inout Position) throws -> Bool {
    let fpos = pos
    var c = input.popChar()
    
    if c != "(" {
        if c != nil { input.pushChar(c!) }
        return false
    }
    
    pos.column += 1
    let items = try readAll(readForm, &input, [], &pos)
    c = input.popChar()

    if c != ")" {
        if c != nil { input.pushChar(c!) }
        throw ReadError.invalidSyntax(fpos)
    }
    
    pos.column += 1
    output.append(List(fpos, items))
    return true
}

func readPair(_ input: inout Input, _ output: inout [Form], _ pos: inout Position) throws -> Bool {
    let fpos = pos
    let c = input.popChar()
    
    if c != ":" {
        if c != nil { input.pushChar(c!) }
        return false
    }
    
    pos.column += 1
    try readWhitespace(&input, &output, &pos)

    if try output.isEmpty || !readForm(&input, &output, &pos) {
        throw ReadError.invalidSyntax(pos)
    }
    
    let right = output.removeLast()
    let left = output.removeLast()
    output.append(Pair(fpos, left, right))
    return true
}

func readReference(_ input: inout Input, _ output: inout [Form], _ pos: inout Position) throws -> Bool {
    let fpos = pos
    let c = input.popChar()
    
    if c != "&" {
        if c != nil { input.pushChar(c!) }
        return false
    }
    
    pos.column += 1

    if !(try readIdentifier(&input, &output, &pos)) {
        throw ReadError.invalidSyntax(fpos)
    }
    
    output.append(Reference(fpos, try output.removeLast().cast(Identifier.self).name))
    return true
}

func readString(_ input: inout Input, _ output: inout [Form], _ pos: inout Position) throws -> Bool {
    let fpos = pos
    var c = input.popChar()
    
    if c != "\"" {
        if c != nil { input.pushChar(c!) }
        return false
    }
    
    pos.column += 1
    var body: [Character] = []

    while true {
        c = input.popChar()
        if c == nil || c == "\"" { break }
        body.append(c!)
    }

    if c != "\"" {
        throw ReadError.invalidSyntax(fpos)
    }
    
    pos.column += 1
    output.append(Literal(fpos, Value(std.stringType, String(body))))
    return true
}

@discardableResult
func readWhitespace(_ input: inout Input, _ output: inout [Form], _ pos: inout Position) throws -> Bool {
    let p = pos
    
    while let c = input.popChar() {
        if c.isNewline {
            pos.line += 1
        } else if c.isWhitespace {
            pos.column += 1
        } else {
            input.pushChar(c)
            break
        }
    }
    
    return pos.line != p.line || pos.column != p.column
}

class StandardLibrary: Namespace {
    class ArgumentType: BasicType<Int>, ValueType {
        init() {
            super.init("Argument")
        }

        func equals(_ value1: Value, _ value2: Value) -> Bool {
            return cast(value1) == cast(value2)
        }

        override func identifierEmit(_ value: Value, _ vm: VM,
                                     at pos: Position, inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
            vm.emit(.argument(cast(value)))
        }
    }

    class ArrayType: BasicType<[Value]>, ValueType {
        init() {
            super.init("Array")
        }

        func equals(_ value1: Value, _ value2: Value) -> Bool {
            let a1 = cast(value1)
            let a2 = cast(value2)
            if a1.count != a2.count { return false }
            
            for i in 0..<a1.count {
                if !a1[i].equals(a2[i]) { return false }
            }
            
            return true
        }

        override func toBool(_ value: Value) -> Bool {
            return cast(value).count != 0
        }

        override func toString(_ value: Value) -> String {
            "[\(cast(value).map({"\($0)"}).joined(separator: " "))]"
        }
    }

    class BoolType: BasicType<Bool>, ValueType {
        init() {
            super.init("Bool")
        }
        
        func equals(_ value1: Value, _ value2: Value) -> Bool {
            return cast(value1) == cast(value2)
        }

        override func toBool(_ value: Value) -> Bool {
            cast(value)
        }

        override func toString(_ value: Value) -> String {
            "\(cast(value))"
        }
    }
    
    class FunctionType: BasicType<Function>, ValueType {
        init() {
            super.init("Function")
        }

        func equals(_ value1: Value, _ value2: Value) -> Bool {
            return cast(value1) === cast(value2)
        }

        override func identifierEmit(_ value: Value, _ vm: VM,
                                     at pos: Position, inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
            let f = cast(value)
            
            for _ in 0..<f.arguments.count {
                if args.isEmpty {
                    throw EmitError.missingArgument(pos)
                }
                
                try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
            }
            
            vm.emit(.call(pos, f))
        }
    }

    class IntType: BasicType<Int>, ValueType {
        init() {
            super.init("Int")
        }

        func equals(_ value1: Value, _ value2: Value) -> Bool {
            return cast(value1) == cast(value2)
        }

        override func toBool(_ value: Value) -> Bool {
            cast(value) != 0
        }
    }

    class MacroType: BasicType<Macro>, ValueType {
        init() {
            super.init("Macro")
        }

        func equals(_ value1: Value, _ value2: Value) -> Bool {
            return cast(value1) === cast(value2)
        }

        override func identifierEmit(_ value: Value, _ vm: VM,
                                     at pos: Position, inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
            try cast(value).emit(vm, at: pos, inNamespace: ns, withArguments: &args)
        }
    }

    class MetaType: BasicType<any ValueType>, ValueType {
        init() {
            super.init("Meta")
        }
        
        func equals(_ value1: Value, _ value2: Value) -> Bool {
            return !cast(value1).equals(cast(value2))
        }
    }

    class PairType: BasicType<(Value, Value)>, ValueType {
        init() {
            super.init("Pair")
        }

        func equals(_ value1: Value, _ value2: Value) -> Bool {
            let p1 = cast(value1)
            let p2 = cast(value2)
            return p1.0.equals(p2.0) && p1.1.equals(p2.1)
        }

        override func toBool(_ value: Value) -> Bool {
            let v = cast(value)
            return v.0.toBool && v.1.toBool
        }

        override func toString(_ value: Value) -> String {
            let p = cast(value)
            return "\(p.0):\(p.1)"
        }
    }

    class StringType: BasicType<String>, ValueType {
        init() {
            super.init("String")
        }

        func equals(_ value1: Value, _ value2: Value) -> Bool {
            return cast(value1) == cast(value2)
        }

        override func toBool(_ value: Value) -> Bool {
            cast(value).count != 0
        }

        override func toString(_ value: Value) -> String {
            "\"\(cast(value))\""
        }
    }

    let argumentType = ArgumentType()
    let arrayType = ArrayType()
    let boolType = BoolType()
    let functionType = FunctionType()
    let intType = IntType()
    let macroType = MacroType()
    let metaType = MetaType()
    let pairType = PairType()
    let stringType = StringType()

    init() {
        super.init()
    
        self["Array"] = Value(metaType, arrayType)
        self["Bool"] = Value(metaType, boolType)
        self["Function"] = Value(metaType, functionType)
        self["Int"] = Value(metaType, intType)
        self["Macro"] = Value(metaType, macroType)
        self["Meta"] = Value(metaType, metaType)
        self["Pair"] = Value(metaType, pairType)
        self["String"] = Value(metaType, stringType)

        self["true"] = Value(boolType, true)
        self["false"] = Value(boolType, false)

        bindMacro("check", 2) {(_, vm, pos, ns, args) throws in
            try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
            vm.emit(.check(pos))
            try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
            vm.emit(.stop)
        }

        bindMacro("function", 2) {(_, vm, pos, ns, args) throws in
            var id: String?
            
            if args.first! is Identifier {
                id = try args.removeFirst().cast(Identifier.self).name
            }

            let fargs = try args.removeFirst().cast(List.self).items.map {(it) in
                let p = try it.cast(Pair.self)
                let n = try p.left.cast(Identifier.self).name
                let tid = try p.right.cast(Identifier.self).name
                let t = ns[tid]

                if t == nil {
                    throw EmitError.unknownIdentifier(p.right.position, tid)
                }
                
                return try (n, self.metaType.safeCast(t!, at: pos))
            }
            
            let body = args.removeFirst()
            let skip = vm.emit(.nop)
            let startPc = vm.emitPc
            
            let f = Function(id ?? "lambda", fargs) {(f, vm, pos) throws in
                vm.callStack.append(Function.Call(f, at: pos, stackOffset: vm.stack.count-fargs.count, returnPc: vm.pc))
                vm.pc = startPc
            }

            if id != nil {
                ns[id!] = Value(self.functionType, f)
            }
            
            let fns = Namespace(ns)
            
            for i in 0..<fargs.count {
                fns[fargs[i].0] = Value(self.argumentType, i)
            }
            
            try body.emit(vm, inNamespace: fns, withArguments: &args)
            vm.emit(.popCall)
            vm.code[skip] = .goto(vm.emitPc)

            if id == nil {
                vm.emit(.push(Value(self.functionType, f)))
            }
        }

        bindMacro("if", 2) {(_, vm, pos, ns, args) throws in
            try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
            let ifPc = vm.emit(.nop)
            try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
            var elsePc = vm.emitPc
            
            if !args.isEmpty {
                if let next = args.first as? Identifier {
                    if next.name == "else" {
                        _ = args.removeFirst()
                        let skipPc = vm.emit(.nop)
                        elsePc = vm.emitPc
                        try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
                        vm.code[skipPc] = .goto(vm.emitPc)
                    }
                }
            }
            
            vm.code[ifPc] = .branch(elsePc)
        }
        
        bindMacro("or", 2) {(_, vm, pos, ns, args) throws in
            try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
            let or = vm.emit(.nop)
            try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
            vm.code[or] = .or(vm.emitPc)
        }
    
        bindMacro("task", 1) {(_, vm, pos, ns, args) throws in
            let task = vm.emit(.nop)
            try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
            vm.emit(.stop)
            vm.code[task] = .task(vm.emitPc)
        }

        bindMacro("trace", 0) {(_, vm, pos, ns, args) throws in
            vm.trace = !vm.trace
        }

        bindFunction("=", [("left", intType), ("right", intType)]) {(_, vm, pos) throws in
            vm.push(Value(self.boolType, vm.pop().equals(vm.pop())))
        }

        bindFunction("<", [("left", intType), ("right", intType)]) {(_, vm, pos) throws in
            let r = self.intType.cast(vm.pop())
            let l = self.intType.cast(vm.pop())
            vm.push(Value(self.boolType, l < r))
        }
        
        bindFunction(">", [("left", intType), ("right", intType)]) {(_, vm, pos) throws in
            let r = self.intType.cast(vm.pop())
            let l = self.intType.cast(vm.pop())
            vm.push(Value(self.boolType, l > r))
        }
        
        bindFunction("+", [("left", intType), ("right", intType)]) {(_, vm, pos) throws in
            let r = self.intType.cast(vm.pop())
            let l = self.intType.cast(vm.pop())
            vm.push(Value(self.intType, l + r))
        }

        bindFunction("-", [("left", intType), ("right", intType)]) {(_, vm, pos) throws in
            let r = self.intType.cast(vm.pop())
            let l = self.intType.cast(vm.pop())
            vm.push(Value(self.intType, l - r))
        }
        
        bindFunction("call", [("target", functionType), ("arguments", arrayType)]) {(_, vm, pos) throws in
            let args = self.arrayType.cast(vm.pop())
            let f = self.functionType.cast(vm.pop())
            if args.count != f.arguments.count { throw EvaluateError.arityMismatch(pos) }

            for i in 0..<f.arguments.count {
                let expected = f.arguments[i].1
                let actual  = args[i].type
                if !actual.equals(expected) { throw EvaluateError.typeMismatch(pos) }
            }

            vm.stack.append(contentsOf: args)
            try f.call(vm, at: pos)
        }

        bindFunction("load", [("path", stringType)]) {(_, vm, pos) throws in
            let startPc = vm.emitPc
            try load(vm, readForm, fromPath: self.stringType.cast(vm.pop()), inNamespace: self)
            vm.emit(.stop)
            try vm.evaluate(fromPc: startPc)
        }
        
        bindFunction("yield", []) {(_, vm, pos) throws in
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

func load(_ vm: VM, _ reader: Reader, fromPath path: String, inNamespace ns: Namespace) throws {
    var input = Input(try String(contentsOfFile: path, encoding: String.Encoding.utf8))
    var pos = Position(path)
    let fs = try readAll(reader, &input, [], &pos)
    try fs.emit(vm, inNamespace: ns)
}

func repl(_ vm: VM, _ reader: Reader, inNamespace ns: Namespace) throws {
    var input = Input()
    var prompt = 1
    
    while true {
        print("\(prompt). ", terminator: "")
        let line = readLine(strippingNewline: false)
        
        if line == nil || line! == "\n" {
            do {
                var pos = Position("repl")
                let fs = try readAll(reader, &input, [], &pos)
                let pc = vm.emitPc
                try fs.emit(vm, inNamespace: ns)
                vm.emit(.stop)
                try vm.evaluate(fromPc: pc)
                print("\(vm.stack.isEmpty ? "_" : "\(vm.pop())")\n")
                input.reset()
            } catch {
                print("\(error)\n")
            }
            
            prompt = 1
        } else {
            input.append(line!)
            prompt += 1
        }
    }
}

/*
 Now we're ready to take it for a spin.

 Here are some ideas to play around with:

 1. + 1 2
 2. 
 3


 1. task 42
 2. 
 _

 1. yield
 2. 
 42
 */

let vm = VM()

if CommandLine.arguments.count == 1 {
    try repl(vm, readForm, inNamespace: std)
} else {
    for p in CommandLine.arguments[1...] {
        let startPc = vm.emitPc
        try load(vm, readForm, fromPath: p, inNamespace: std)
        vm.emit(.stop)
        try vm.evaluate(fromPc: startPc)
    }
}
