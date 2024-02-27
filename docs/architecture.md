## Main
- When *fmlisp* starts, there are two execution options:
    1. REPL
    2. load and execute a file

## Environment
- An environment keeps track of namespaces
- The `eval` code is located inside the environment to be easy accessible

## Namespace
- A namespace has a Symbol
- A namespace contains a mapping of Symbols-Vars

## Interop
```
(mymod::fn 1 2 3)
```
```rust
mymod::fn(1, 2, 3)
```
