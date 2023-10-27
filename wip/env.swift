public class Env {
    var bindings: [String:Val] = [:]

    public func bind(_ name: String, to val: Val) {
        bindings[name] = val
    }
}
