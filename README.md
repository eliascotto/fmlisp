## FMLisp

A fast and modern Lisp. Fast since is built on top of Rust, Modern because it uses Clojure modern lisp sintax. A mix between Common Lisp performance and power, and Clojure modern features.

## Guidelines

- Keep it simple, always prefer the easy solution
- Optimization comes later
- Prioritize what matters
- Write maintanable and generic tests

## TODO

- Fix `set-macro` and test `defn`
- `refer` function
- `require` with file loading
- `doc`
- Add support for keyword arguments
- Proc Macro to define Rust functions as plugins and load them into FMLisp at the startup
- Start UI with GPUI
- Add ns exploring REPL functions as `ns-intern` (in a REPL namespace?)
- Lazy evaluation
- Errors with line numbers and/or stack traces. Options to print backtrace in case of errors
- Exception system
- Lazy sequences `lazy-cons`
- Clojure-style protocols
- Full call/cc (call-with-current-continuation) support
- Explicit TCO (i.e. recur) with tail-position error checking
- Functions to debug
- Move List to use proper linked list implementation
- Load/Save(dump) an environment like CL
- REPL readline input only if parentesis are fully closed

## Features

- Condition system with REPL block (enable/disable) and RESTARTS
- `ignore-errors` from CL
- Lazy evaluation
- Fast lists structures
- Immutable data structures
- Better stacktrace and errors
  - error show on line like rust
- Static typing?? - if it's worth it

## Specials Forms

- &
- monitor-exit
- case\*
- try/catch
- reify\*
- finally
- loop\*
- letfn\*
- clojure.core/import\*
- new
- deftype\*
- let\*
- fn\*
- recur
- set!
- .
- quote
- catch
- throw
- monitor-enter

## Future

- Add `&form &env` to `defmacro` internal `defn` arguments
- Add `set!` to force setting a var, overriding the immutability

## General

- Replace Excel, SQL, Postico, Pandas
- This Lisp is faster

### MAL 2.0

- structural comaprison (so that we can write tests in the language itself aka assert)
- continuations
- pattern matching and destructuring
- concurency (event loop)
- parallelism (threads or actors)
- persistent data structures. See MIT course
- compiler aka AOT compiler
- JIT compiler
- garbage collector
- Lisp without garbage collector aka “soft real time”. See Carp, bone-lisp
- lazy evaluation
- module system (and namespaces)
- named parameters for functions
- dynamic type checker aka guards or design by contract
- static type checker (gradual type system?)
- excpetions with stack traces
- partial application aka auto-currying]
- pattern matching on function params (like in Shen)
- logic programming (prolog or minikanren)
- alternative to regular expressions
- function overloading
- user defined types and/or data structures
- effect system
- better error messages
- REPL improvements, for example, show all currently defined variables, show documentation for a function, show infered types, etc.
