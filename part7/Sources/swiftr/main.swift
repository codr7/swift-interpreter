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

    func identifierEmit(_ vm: VM,
                        at pos: Position,
                        inNamespace ns: Namespace,
                        withArguments args: inout [Form]) throws {
        try self.type.identifierEmit(self, vm, at: pos, inNamespace: ns, withArguments: &args)
    }
}

protocol ValueType: CustomStringConvertible {
    associatedtype V
    
    var name: String { get }

    func cast(_ value: Value) -> V
    func equals(_ other: any ValueType) -> Bool
    
    func identifierEmit(_ value: Value,
                        _ vm: VM,
                        at: Position,
                        inNamespace: Namespace,
                        withArguments args: inout [Form]) throws

    func safeCast(_ value: Value, at: Position) throws -> V
    func toBool(_ value: Value) -> Bool
    func toString(_ value: Value) -> String
}

class BasicValueType<V>: ValueType {
    let name: String
    var description: String { name }

    init(_ name: String) {
        self.name = name
    }

    func cast(_ value: Value) -> V {
        value.data as! V
    }

    func equals(_ other: any ValueType) -> Bool {
        if let rhs = other as? BasicValueType<V> {
            return self === rhs
        }

        return false
    }

    func identifierEmit(_ value: Value,
                        _ vm: VM,
                        at pos: Position,
                        inNamespace: Namespace,
                        withArguments args: inout [Form]) throws {
        args.insert(Literal(pos, value), at: 0)
    }

    func safeCast(_ value: Value, at pos: Position) throws -> V {
        let v = value.data as? V

        if v == nil {
            throw EvaluateError.typeMismatch(pos, self, value.type)
        }

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
    case typeMismatch(Position, any ValueType, any ValueType)
    case unknownIdentifier(Position, String)
}

enum EvaluateError: Error {
    case arityMismatch(Position, Int, Int)
    case missingValue(Position)
    case typeMismatch(Position, any ValueType, any ValueType)
}

enum ReadError: Error {
    case invalidSyntax(Position)
}

typealias PC = Int

struct Argument {
    struct Value: Equatable {
        let callOffset: Int
        let index: Int
        
        init(_ callOffset: Int, _ index: Int) {
            self.callOffset = callOffset
            self.index = index
        }
    }

    let value: Value
    let type: any ValueType

    init(_ callOffset: Int, _ index: Int, _ type: any ValueType) {
        self.value = Value(callOffset, index)
        self.type = type
    }
}

class Call {
    let parentCall: Call?
    let returnPc: PC
    
    var position: Position
    var stackOffset: Int
    var target: Function
    
    init(_ parentCall: Call?, _ target: Function, at pos: Position, stackOffset: Int, returnPc: PC) {
        self.parentCall = parentCall
        self.target = target
        self.position = pos
        self.stackOffset = stackOffset
        self.returnPc = returnPc
    }
}

class Function: CustomStringConvertible {
    typealias Body = (Function, VM, inout PC, inout Stack, Position) throws -> Void
    
    let arguments: [(String, any ValueType)]
    let body: Body
    let name: String
    let resultType: (any ValueType)?
    let startPc: PC?
    var endPc: PC?
    
    lazy var closureArguments: [Argument.Value] = {
        var result: [Argument.Value] = []

        if startPc != nil {
            for i in startPc!..<endPc! {
                switch vm.code[i] {
                case let .argument(arg):
                    if arg.value.callOffset > 0 {
                        let ai = Argument.Value(arg.value.callOffset-1, arg.value.index)
                        
                        if result.firstIndex(of: ai) == nil {
                            result.append(ai)
                        }
                        
                        vm.code[i] = .argument(Argument(0,
                                                        arguments.count + result.firstIndex(of: ai)!,
                                                        arg.type))
                    }
                default:
                    break
                }
            }
        }

        return result
    }()
    
    var description: String { name }
    
    init(_ name: String,
         _ arguments: [(String, any ValueType)],
         _ resultType: (any ValueType)?,
         startPc: PC? = nil,
         endPc: PC? = nil,
         _ body: @escaping Body) {
        self.name = name
        self.arguments = arguments
        self.resultType = resultType
        self.startPc = startPc
        self.endPc = endPc
        self.body = body
    }

    func call(_ vm: VM, _ pc: inout PC, _ stack: inout Stack, at pos: Position) throws {
        try body(self, vm, &pc, &stack, pos)
    }

    func stackOffset(_ stack: Stack) -> Int {
        stack.count - arguments.count
    }
}

class Closure: Function {
    let stack: Stack
    
    init(_ target: Function, _ stack: Stack) {
        self.stack = stack

        super.init(target.name,
                   target.arguments,
                   target.resultType,
                   startPc: target.startPc,
                   endPc: target.endPc,
                   target.body)
        
        self.closureArguments = target.closureArguments
    }
    
    override func call(_ vm: VM, _ pc: inout PC, _ stack: inout Stack, at pos: Position) throws {
        stack.append(contentsOf: self.stack)
        try super.call(vm, &pc, &stack, at: pos)
    }

    override func stackOffset(_ stack: Stack) -> Int {
        stack.count - closureArguments.count - arguments.count
    }
}

struct Macro: CustomStringConvertible {
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

    func emit(_ vm: VM,
              at pos: Position,
              inNamespace ns: Namespace,
              withArguments args: inout [Form]) throws {
        if args.count < arity {
            throw EmitError.missingArgument(pos)
        }

        try body(self, vm, pos, ns, &args)
    }
}

class Namespace: Sequence {
    typealias Bindings = [String:Value]
    typealias Iterator = Bindings.Iterator
    
    let parent: Namespace?
    var bindings: Bindings = [:]
    
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

    public func makeIterator() -> Iterator {
        bindings.makeIterator()
    }
}

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

enum EmitOption  {
    case tailCall
}

protocol Form: CustomStringConvertible {
    var position: Position {get}
    func cast<T: Form>(_ type: T.Type) throws -> T
    func emit(_ vm: VM,
              inNamespace: Namespace,
              withArguments: inout [Form]) throws
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
              inNamespace ns: Namespace,
              withArguments args: inout [Form]) throws {
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
              inNamespace ns: Namespace,
              withArguments args: inout [Form]) throws {
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
              inNamespace ns: Namespace,
              withArguments args: inout [Form]) throws {
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
              inNamespace ns: Namespace,
              withArguments args: inout [Form]) throws {
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

    func emit(_ vm: VM,
              inNamespace ns: Namespace,
              withArguments args: inout [Form]) throws {
        if left is Literal && right is Literal {
            args.insert(Literal(position, Value(std.pairType,
                                                ((left as! Literal).value, (right as! Literal).value))),
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

extension Stack {
    mutating func pop() -> Value {
        removeLast()
    }

    mutating func push(_ value: Value) {
        append(value)
    }
}

class Task {
    typealias Id = Int

    let id: Id

    var currentCall: Call?
    var pc: PC
    var stack: Stack = []

    init(id: Id, startPc: PC) {
        self.id = id
        self.pc = startPc
    }
}

class VM {    
    enum Operation {
        case argument(Argument)
        case benchmark(Position, PC)
        case branch(PC)
        case call(Position, Function)
        case checkType(Position, any ValueType)
        case closure(Function)
        case goto(PC)
        case makeArray(Int)
        case makePair
        case nop
        case or(PC)
        case popCall
        case push(Value)
        case stop
        case tailCall(Position, Function)
        case task(PC)
        case trace
    }

    var code: [Operation] = []

    var currentCall: Call? {
        get {currentTask!.currentCall}
        set(v) {currentTask!.currentCall = v} 
    }

    var currentTask: Task? {tasks[0]}
    var emitPc: PC {code.count}
    var nextTaskId = 0
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
    
    func evaluate(fromPc: PC, stack: inout Stack) throws {
        var pc = fromPc
        
        loop: while true {
            let op = code[pc]
            
            switch op {
            case let .argument(arg):
                var c = currentCall!
                for _ in 0..<arg.value.callOffset { c = c.parentCall! }
                stack.push(stack[c.stackOffset+arg.value.index])
                pc += 1
            case let .branch(elsePc):
                pc = stack.pop().toBool ? pc + 1 : elsePc
            case let .benchmark(pos, endPc):
                if stack.isEmpty { throw EvaluateError.missingValue(pos) }
                let n = stack.pop()

                let t = try ContinuousClock().measure {
                    pc += 1
                    let startPc = pc
                    let stackLength = stack.count
                    
                    for _ in 0..<(try std.intType.safeCast(n, at: pos)) {
                        try evaluate(fromPc: startPc, stack: &stack)
                        stack.removeLast(stack.count - stackLength)
                    }
                }

                stack.push(Value(std.timeType, t))
                pc = endPc
            case let .call(pos, target):
                pc += 1
                try target.call(self, &pc, &stack, at: pos)
            case let .checkType(pos, expected):
                let actual = stack.last!.type
                if !actual.equals(expected) { throw EvaluateError.typeMismatch(pos, expected, actual) }
                pc += 1
            case let .closure(target):
                var cs: Stack = []
                
                for a in target.closureArguments {
                    var c = currentCall!
                    for _ in 0..<a.callOffset { c = c.parentCall! }
                    cs.push(stack[c.stackOffset+a.index])
                }

                stack.push(Value(std.functionType, Closure(target, cs)))
                pc += 1
            case let .goto(targetPc):
                pc = targetPc
            case let .makeArray(count):
                let items = stack.suffix(count)
                stack.removeLast(count)
                stack.push(Value(std.arrayType, Array(items)))
                pc += 1
            case .makePair:
                let right = stack.pop()
                let left = stack.pop()
                stack.push(Value(std.pairType, (left, right)))
                pc += 1
            case .nop:
                pc += 1
            case let .or(endPc):
                pc = stack.pop().toBool ? endPc : pc + 1
            case .popCall:
                let c = currentCall!
                currentCall = c.parentCall
                stack.removeSubrange(c.stackOffset..<c.stackOffset+c.target.arguments.count)
                pc = c.returnPc
            case let .push(value):
                stack.push(value)
                pc += 1
            case .stop:
                pc += 1
                break loop
            case let .tailCall(pos, target):
                let c = currentCall
                
                if c == nil || c!.target.startPc == nil {
                    pc += 1
                    try target.call(self, &pc, &stack, at: pos)
                } else {
                    c!.target = target
                    c!.position = pos
                    c!.stackOffset = stack.count - target.arguments.count
                    pc = target.startPc!
                }
            case let .task(endPc):
                startTask(pc: pc+1)
                pc = endPc
            case .trace:
                pc += 1
                print("\(pc) \(code[pc])")
            }
        }
    }

    func startTask(pc: PC = 0) {
        let t = Task(id: nextTaskId, startPc: pc)
        tasks.append(t)
        nextTaskId += 1
    }
}

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

typealias Reader = (_ input: inout Input, _ output: inout [Form], _ pos: inout Position) throws -> Bool

let readers = [readWhitespace,
               readDot,
               readReference,
               readPair,
               readArray,
               readList,
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
    class ArgumentType: BasicValueType<Argument> {
        init() {
            super.init("Argument")
        }
        
        override func identifierEmit(_ value: Value,
                                     _ vm: VM,
                                     at pos: Position,
                                     inNamespace ns: Namespace,
                                     withArguments args: inout [Form]) throws {
            let a = cast(value)
            vm.emit(.argument(a))
        }
    }

    class ArrayType: BasicValueType<[Value]> {
        init() {
            super.init("Array")
        }
        
        override func toBool(_ value: Value) -> Bool {
            cast(value).count != 0
        }

        override func toString(_ value: Value) -> String {
            "[\(cast(value).map({"\($0)"}).joined(separator: " "))]"
        }
    }

    class BoolType: BasicValueType<Bool> {
        init() {
            super.init("Bool")
        }
        
        override func toBool(_ value: Value) -> Bool {
            cast(value)
        }

        override func toString(_ value: Value) -> String {
            "\(cast(value))"
        }
    }
    
    class FunctionType: BasicValueType<Function> {
        init() {
            super.init("Function")
        }
        
        override func identifierEmit(_ value: Value,
                                     _ vm: VM,
                                     at pos: Position,
                                     inNamespace ns: Namespace,
                                     withArguments args: inout [Form]) throws {
            let f = cast(value)
            
            for a in f.arguments {
                if args.isEmpty {
                    throw EmitError.missingArgument(pos)
                }

                let f = args.removeFirst()
                let expected = a.1
                var actual: (any ValueType)?
                
                if f is Literal {
                    actual = (f as! Literal).value.type
                } else if f is Identifier {
                    let v = ns[(f as! Identifier).name]

                    if v != nil {
                        if v!.type.equals(std.argumentType) {
                            actual = std.argumentType.cast(v!).type
                        } else if v!.type.equals(std.functionType) {
                            actual = std.functionType.cast(v!).resultType
                        }
                    }
                }

                if actual != nil {
                    if !actual!.equals(expected) {
                        throw EmitError.typeMismatch(f.position, expected, actual!)
                    }
                }
                
                try f.emit(vm, inNamespace: ns, withArguments: &args)
                if actual == nil { vm.emit(.checkType(f.position, expected)) }
            }

            vm.emit(.call(pos, f))
        }
    }

    class IntType: BasicValueType<Int> {
        init() {
            super.init("Int")
        }
        
        override func toBool(_ value: Value) -> Bool {
            cast(value) != 0
        }
    }

    class MacroType: BasicValueType<Macro> {
        init() {
            super.init("Macro")
        }
        
        override func identifierEmit(_ value: Value,
                                     _ vm: VM,
                                     at pos: Position,
                                     inNamespace ns: Namespace,
                                     withArguments args: inout [Form]) throws {
            try cast(value).emit(vm, at: pos, inNamespace: ns, withArguments: &args)
        }
    }

    class PairType: BasicValueType<(Value, Value)> {
        init() {
            super.init("Pair")
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

    class StringType: BasicValueType<String> {
        init() {
            super.init("String")
        }
        
        override func toBool(_ value: Value) -> Bool {
            cast(value).count != 0
        }

        override func toString(_ value: Value) -> String {
            "\"\(cast(value))\""
        }
    }

    class TimeType: BasicValueType<Duration> {
        init() {
            super.init("Time")
        }
        
        override func toBool(_ value: Value) -> Bool {
            cast(value) != Duration.zero
        }
    }
    
    let argumentType = ArgumentType()
    let arrayType = ArrayType()
    let boolType = BoolType()
    let functionType = FunctionType()
    let intType = IntType()
    let macroType = MacroType()
    let metaType = BasicValueType<any ValueType>("Meta")
    let pairType = PairType()
    let stringType = StringType()
    let timeType = TimeType()

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
        self["Time"] = Value(metaType, timeType)

        self["true"] = Value(boolType, true)
        self["false"] = Value(boolType, false)
        
        bindMacro("benchmark", 2) {(_, vm, pos, ns, args) throws in
            try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
            let benchmarkPc = vm.emit(.nop)
            try args.removeFirst().emit(vm, inNamespace: Namespace(ns), withArguments: &args)
            vm.emit(.stop)
            vm.code[benchmarkPc] = .benchmark(pos, vm.emitPc)
        }
        
        bindMacro("define", 2) {(_, vm, pos, ns, args) throws in
            let name = try args.removeFirst().cast(Identifier.self).name
            let gotoPc = vm.emit(.nop)
            let startPc = vm.emitPc
            try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
            vm.emit(.stop)
            vm.code[gotoPc] = .goto(vm.emitPc)
            var stack: Stack = []
            try vm.evaluate(fromPc: startPc, stack: &stack)
            
            if stack.isEmpty {
                throw EvaluateError.missingValue(pos)
            }

            ns[name] = stack.pop()
        }

        bindMacro("evaluate", 1) {(_, vm, pos, ns, args) throws in
            let gotoPc = vm.emit(.nop)
            let startPc = vm.emitPc
            try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
            vm.emit(.stop)
            vm.code[gotoPc] = .goto(vm.emitPc)
            var stack: Stack = []
            try vm.evaluate(fromPc: startPc, stack: &stack)

            for v in stack {
                args.insert(Literal(pos, v), at: 0)
            }
        }

        bindMacro("function", 2) {(_, vm, pos, ns, args) throws in
            var id: String?
            
            if args.first! is Identifier {
                id = try args.removeFirst().cast(Identifier.self).name
            }

            var argsForm = args.removeFirst()
            var resultType: (any ValueType)?
            
            if argsForm is Pair {
                let p = argsForm as! Pair
                let tid = try p.right.cast(Identifier.self).name
                let t = ns[tid]
                resultType = try self.metaType.safeCast(t!, at: pos)
                argsForm = p.left
            }
            
            let fargs = try argsForm.cast(List.self).items.map {(it) in
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
            
            let f = Function(id ?? "lambda", fargs, resultType, startPc: startPc) {
                (f, vm, pc, stack, pos) throws in
                vm.currentCall = Call(vm.currentCall, f,
                                      at: pos,
                                      stackOffset: f.stackOffset(stack),
                                      returnPc: pc)
                pc = startPc
            }

            if id != nil {
                ns[id!] = Value(self.functionType, f)
            }
            
            let fns = Namespace(ns)

            for (k, v) in ns {
                if v.type.equals(std.argumentType) {
                    let a = std.argumentType.cast(v)
                    fns[k] = Value(std.argumentType, Argument(a.value.callOffset+1, a.value.index, a.type))
                }
            }
            
            for i in 0..<fargs.count {
                let a = fargs[i]
                fns[a.0] = Value(self.argumentType, Argument(0, i, a.1))
            }

            try body.emit(vm, inNamespace: fns, withArguments: &args)
            vm.emit(.popCall)
            f.endPc = vm.emitPc
            vm.code[skip] = .goto(f.endPc!)

            if id == nil {
                if f.closureArguments.isEmpty {
                    vm.emit(.push(Value(self.functionType, f)))
                } else {
                    vm.emit(.closure(f))
                }
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

        bindMacro("return", 1) {(_, vm, pos, ns, args) throws in
            try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)

            switch vm.code.last {
            case let .call(pos, target):
                if target.startPc != nil {
                    vm.code[vm.code.count-1] = .tailCall(pos, target)
                } else {
                    fallthrough
                }
            default:
                vm.emit(.popCall)
                break
            }
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

        bindFunction("=", [("left", intType), ("right", intType)], boolType) {(_, vm, pc, stack, pos) throws in
            let r = self.intType.cast(stack.pop())
            let l = self.intType.cast(stack.pop())
            stack.push(Value(self.boolType, l == r))
        }

        bindFunction("<", [("left", intType), ("right", intType)], boolType) {(_, vm, pc, stack, pos) throws in
            let r = self.intType.cast(stack.pop())
            let l = self.intType.cast(stack.pop())
            stack.push(Value(self.boolType, l < r))
        }
        
        bindFunction(">", [("left", intType), ("right", intType)], boolType) {(_, vm, pc, stack, pos) throws in
            let r = self.intType.cast(stack.pop())
            let l = self.intType.cast(stack.pop())
            stack.push(Value(self.boolType, l > r))
        }
        
        bindFunction("+", [("left", intType), ("right", intType)], intType) {(_, vm, pc, stack, pos) throws in
            let r = self.intType.cast(stack.pop())
            let l = self.intType.cast(stack.pop())
            stack.push(Value(self.intType, l + r))
        }

        bindFunction("-", [("left", intType), ("right", intType)], intType) {(_, vm, pc, stack, pos) throws in
            let r = self.intType.cast(stack.pop())
            let l = self.intType.cast(stack.pop())
            stack.push(Value(self.intType, l - r))
        }

        bindFunction("call", [("target", functionType), ("arguments", arrayType)], nil) {
            (_, vm, pc, stack, pos) throws in
            let args = self.arrayType.cast(stack.pop())
            let f = self.functionType.cast(stack.pop())

            if args.count != f.arguments.count {
                throw EvaluateError.arityMismatch(pos, f.arguments.count, args.count)
            }


            for i in 0..<f.arguments.count {
                let expected = f.arguments[i].1
                let actual  = args[i].type
                
                if !actual.equals(expected) {
                    throw EvaluateError.typeMismatch(pos, expected, actual)
                }
            }

            stack.append(contentsOf: args)
            try f.call(vm, &pc, &stack, at: pos)
        }
        
        bindFunction("load", [("path", stringType)], nil) {(_, vm, pc, stack, pos) throws in
            let startPc = vm.emitPc
            try load(vm, readForm, fromPath: self.stringType.cast(stack.pop()), inNamespace: self)
            vm.emit(.stop)
            try vm.evaluate(fromPc: startPc, stack: &stack)
        }

        bindFunction("milliseconds", [("value", intType)], timeType) {(_, vm, pc, stack, pos) throws in
            let v = self.intType.cast(stack.pop())
            stack.push(Value(self.timeType, Duration.milliseconds(v)))
        }

        bindFunction("sleep", [("duration", timeType)], nil) {(_, vm, pc, stack, pos) throws in
            let d = self.timeType.cast(stack.pop())
            Thread.sleep(forTimeInterval: Double(d.components.seconds) +
                           Double(d.components.attoseconds) * 1e-18)
        }

        bindFunction("yield", [], nil) {(_, vm, pc, stack, pos) throws in
            let c1 = vm.currentTask!
            c1.pc = pc
            c1.stack = stack
            vm.tasks.append(vm.tasks.removeFirst())
            let c2 = vm.currentTask!
            pc = c2.pc
            stack = c2.stack
        }
    }

    func bindFunction(_ name: String, _ args: [(String, any ValueType)], _ resultType: (any ValueType)?,_ body: @escaping Function.Body) {
        self[name] = Value(functionType, Function(name, args, resultType, body))
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

func repl(_ vm: VM, _ reader: Reader, inNamespace ns: Namespace, stack: inout Stack) throws {
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
                try vm.evaluate(fromPc: pc, stack: &stack)
                print("\(stack.isEmpty ? "_" : "\(stack.pop())")\n")
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

let vm = VM()
var stack: Stack = []

if CommandLine.arguments.count == 1 {
    try repl(vm, readForm, inNamespace: std, stack: &stack)
} else {
    for p in CommandLine.arguments[1...] {
        let startPc = vm.emitPc
        try load(vm, readForm, fromPath: p, inNamespace: std)
        vm.emit(.stop)
        try vm.evaluate(fromPc: startPc, stack: &stack)
    }
}
