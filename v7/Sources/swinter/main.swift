/*
 Version 7
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

    func identifierEmit(_ vm: VM, at pos: Position,
                        inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        try self.type.identifierEmit(self, vm, at: pos, inNamespace: ns, withArguments: &args)
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

    func identifierEmit(_ value: Value, _ vm: VM, at: Position,
                        inNamespace: Namespace, withArguments: inout [Form]) throws {
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
    case missingArgument(Position)
    case unknownIdentifier(Position, String)
}

enum EvalError: Error {
    case missingValue(Position)
}

enum ReadError: Error {
    case openList(Position)
    case openString(Position)
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
    var description: String { name }
    
    init(_ name: String, _ arguments: [String], _ body: @escaping Body) {
        self.name = name
        self.arguments = arguments
        self.body = body
    }

    func call(_ vm: VM, at pos: Position) throws {
        if vm.stack.count < arguments.count {
            throw EvalError.missingValue(pos)
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
    typealias Body = (Macro, VM, Position, Namespace, inout [Form]) throws -> Void
    
    let name: String
    let body: Body

    var description: String { name }
    
    init(_ name: String, _ body: @escaping Body) {
        self.name = name
        self.body = body
    }

    func emit(_ vm: VM, at pos: Position, inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        try body(self, vm, pos, ns, &args)
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

/*
 Forms are the bits and pieces that the syntax consists of, the initial step in converting code to 
 operations.
 */

protocol Form: CustomStringConvertible {
    var position: Position {get}
    func emit(_ vm: VM, inNamespace: Namespace, withArguments: inout [Form]) throws
}

class BasicForm {
    let position: Position
    var description: String { "\(self)" }

    init(_ position: Position) {
        self.position = position
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
    case benchmark(Position)
    case branch(Position, PC)
    case call(Position, Function)
    case goto(PC)
    case nop
    case or(Position, PC)
    case popCall(Function)
    case push(Value)
    case stop
    case task(PC)
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
            case let .benchmark(pos):
                if stack.isEmpty {
                    throw EvalError.missingValue(pos)
                }

                let n = pop()

                let t = try ContinuousClock().measure {
                    pc += 1
                    let startPc = pc
                    let stackLength = stack.count
                    
                    for _ in 0..<(n.data as! Int) {
                        try eval(fromPc: startPc)
                        stack.removeLast(stack.count - stackLength)
                    }
                }

                push(Value(timeType, t))
            case let .call(pos, target):
                pc += 1
                try target.call(self, at: pos)
            case let .goto(targetPc):
                pc = targetPc
            case let .branch(pos, elsePc):
                if stack.isEmpty {
                    throw EvalError.missingValue(pos)
                }

                if pop().toBool {
                    pc += 1
                } else {
                    pc = elsePc
                }
            case .nop:
                pc += 1
            case let .or(pos, endPc):
                if let l = peek() {
                    if l.toBool {
                        pc = endPc
                    } else {
                        _ = pop()
                        pc += 1
                    }
                } else {
                    throw EvalError.missingValue(pos)
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
            case let .task(endPc):
                startTask(pc: pc+1)
                pc = endPc
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

typealias Reader = (_ input: inout Input, _ pos: inout Position) throws -> Form?

let readers = [readWhitespace, readInt, readList, readString, readIdentifier]

func readForm(_ input: inout Input, _ pos: inout Position) throws -> Form? {
    for r in readers {
        if let f = try r(&input, &pos) {
            return f
        }
    }

    return nil
}

func readForms(_ reader: Reader, _ input: inout Input, _ pos: inout Position) throws -> [Form] {
    var output: [Form] = []
    
    while let f = try reader(&input, &pos) {
        output.append(f)
    }

    return output
}

func readIdentifier(_ input: inout Input, _ pos: inout Position) throws -> Form? {
    let fpos = pos
    var name = ""
    
    while let c = input.popChar() {
        if c.isWhitespace || c == "(" || c == ")" {
            input.pushChar(c)
            break
        }
        
        name.append(c)
        pos.column += 1
    }
    
    return (name.count == 0) ? nil : Identifier(fpos, name)
}

func readInt(_ input: inout Input, _ pos: inout Position) throws -> Form? {
    let fpos = pos
    var v = 0
    var neg = false
    
    let c = input.popChar()
    if c == nil { return nil }
    
    if c == "-" {
        if let c = input.popChar() {
            if c.isNumber {
                neg = true
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
    
    return (pos.column == fpos.column) ? nil : Literal(fpos, Value(intType, neg ? -v : v))
}

func readList(_ input: inout Input, _ pos: inout Position) throws -> Form? {
    let fpos = pos
    var c = input.popChar()
    
    if c != "(" {
        if c != nil { input.pushChar(c!) }
        return nil
    }
    
    pos.column += 1
    var items: [Form] = []

    while true {
        _ = try readWhitespace(&input, &pos)
        c = input.popChar()
        if c == nil || c == ")" { break }
        input.pushChar(c!)

        if let f = try readForm(&input, &pos) {
            items.append(f)
        } else {
            break
        }
    }

    if c != ")" { throw ReadError.openList(fpos) }
    pos.column += 1
    
    return List(fpos, items)
}

func readString(_ input: inout Input, _ pos: inout Position) throws -> Form? {
    let fpos = pos
    var c = input.popChar()
    
    if c != "\"" {
        if c != nil { input.pushChar(c!) }
        return nil
    }
    
    pos.column += 1
    var body: [Character] = []

    while true {
        c = input.popChar()
        if c == nil || c == "\"" { break }
        body.append(c!)
    }

    if c != "\"" { throw ReadError.openString(fpos) }
    pos.column += 1
    return Literal(fpos, Value(stringType, String(body)))
}

func readWhitespace(_ input: inout Input, _ pos: inout Position) throws -> Form? {
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
    
    return nil
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

    override func identifierEmit(_ value: Value, _ vm: VM, at pos: Position,
                                 inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        vm.emit(.argument(value.data as! Int))
    }
}

let argumentType = ArgumentType()

class BoolType: ValueType {
    init() {
        super.init("Bool")
    }

    override func toBool(_ value: Value) -> Bool {
        value.data as! Bool
    }
}

let boolType = BoolType()
stdLib["Bool"] = Value(metaType, boolType)
stdLib["true"] = Value(boolType, true)
stdLib["false"] = Value(boolType, false)

class FunctionType: ValueType {
    init() {
        super.init("Function")
    }

    override func identifierEmit(_ value: Value, _ vm: VM, at pos: Position,
                                 inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        let f = value.data as! Function
        
        for _ in 0..<f.arguments.count {
            if args.isEmpty {
                throw EmitError.missingArgument(pos)
            }

            try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
        }

        vm.emit(.call(pos, f))
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

    override func identifierEmit(_ value: Value, _ vm: VM, at pos: Position,
                                 inNamespace ns: Namespace, withArguments args: inout [Form]) throws {
        try (value.data as! Macro).emit(vm, at: pos, inNamespace: ns, withArguments: &args)
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

class TimeType: ValueType {
    init() {
        super.init("Time")
    }

    override func toBool(_ value: Value) -> Bool {
        (value.data as! Duration) != Duration.zero
    }
}

let timeType = TimeType()
stdLib["Time"] = Value(metaType, timeType)

stdMacro("benchmark") {(_, vm, pos, ns, args) throws in
    try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
    vm.emit(.benchmark(pos))
    try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
    vm.emit(.stop)
}

stdMacro("function") {(_, vm, pos, ns, args) throws in
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

stdMacro("if") {(_, vm, pos, ns, args) throws in
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

    vm.code[ifPc] = .branch(pos, elsePc)
}

stdMacro("or") {(_, vm, pos, ns, args) throws in
    try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
    let orPc = vm.emit(.nop)
    try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
    vm.code[orPc] = .or(pos, vm.emitPc)
}

stdMacro("trace") {(_, vm, pos, ns, args) throws in
    vm.trace = !vm.trace
}

stdMacro("task") {(_, vm, pos, ns, args) throws in
    let task = vm.emit(.nop)
    try args.removeFirst().emit(vm, inNamespace: ns, withArguments: &args)
    vm.emit(.stop)
    vm.code[task] = .task(vm.emitPc)
}

stdFunction("=", ["left", "right"]) {(_, vm) throws in
    let r = vm.pop()
    let l = vm.pop()
    vm.push(Value(boolType, (l.data as! Int) == (r.data as! Int)))
}

stdFunction("<", ["left", "right"]) {(_, vm) throws in
    let r = vm.pop()
    let l = vm.pop()
    vm.push(Value(boolType, (l.data as! Int) < (r.data as! Int)))
}

stdFunction(">", ["left", "right"]) {(_, vm) throws in
    let r = vm.pop()
    let l = vm.pop()
    vm.push(Value(boolType, (l.data as! Int) > (r.data as! Int)))
}

stdFunction("+", ["left", "right"]) {(_, vm) throws in
    let r = vm.pop()
    let l = vm.pop()
    vm.push(Value(intType, (l.data as! Int) + (r.data as! Int)))
}

stdFunction("-", ["left", "right"]) {(_, vm) throws in
    let r = vm.pop()
    let l = vm.pop()
    vm.push(Value(intType, (l.data as! Int) - (r.data as! Int)))
}

stdFunction("yield", []) {(_, vm) throws in
    vm.tasks.append(vm.tasks.removeFirst())
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
                let fs = try readForms(reader, &input, &pos)
                let pc = vm.emitPc
                try fs.emit(vm, inNamespace: ns)
                vm.emit(.stop)
                try vm.eval(fromPc: pc)
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

 We'll implement two flavors of Fibonacci, one recursive and one tail recursive:

1. function fib1(n) if < n 2 n else + fib1 - n 1 fib1 - n 2
2. fib1 10
3.
55

1. function fib2(n a b) if > n 1 fib2 - n 1 b + a b else if = n 0 a else b
2. fib2 10 0 1
3.
55
 */

let vm = VM()
try repl(vm, readForm, inNamespace: stdLib)
