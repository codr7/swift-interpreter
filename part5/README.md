# 5. Readers and REPL

## Setup
To try it out; simply download and install [Swift](https://www.swift.org/download/) run `swift main.swift` from this directory.

## constants
Constants allow naming values at compile time.

```
stdMacro("constant", 2) {(_, vm, pos, ns, args) throws in
    let name = (args.removeFirst() as! Identifier).name
    let value = (args.removeFirst() as! Literal).value
    ns[name] = value
}
```

```
1. constant foo 42
2. 
_

1. foo
2. 
42
```