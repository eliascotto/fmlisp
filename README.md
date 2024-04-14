## FMLisp

A fast and modern Lisp. Fast since is built on top of Rust, Modern because it uses Clojure modern lisp sintax. A mix between Common Lisp performance and power, and Clojure modern features.

## Guidelines

- Keep it simple, always prefer the easy solution
- Optimization comes later
- Prioritize what matters
- Write maintanable and generic tests

## Language

- Syntactically modern as Clojure
- Fast since based on Rust
- No memory management involved
- Simple language, Clojure gets too complex, it becomes difficult to learn and write proper idiomatic code. In the contrary, Rust is simple and quick to grasp
  - One performant way to iterate over lists
  - Deep thinking before adding lazy loading
  -
- Keep the core library limited and split the extra in alternative libraries to import separately.

## TODO

- Add incremental ID to local ENVS
- Add `recur` support
- Add support for _out_ variable and printing
- fixing tests
- `refer` function
- `require` with file loading
- `doc`
- support for custom `struct` implementation and instance
- Proc Macro to define Rust functions as plugins and load them into FMLisp at the startup
- Add support for keyword arguments
- Start UI with GPUI
- Add line,column numbers to Reader output and to Eval error messages
- Add ns exploring REPL functions as `ns-intern` (in a REPL namespace?)
- Better error messages with problem printing, line numbers and guide to fix
- Function arguments with types as Rust
- Exception system
- Load/Save(dump) an environment like CL
- Clojure-style protocols
- Full call/cc (call-with-current-continuation) support
- Explicit TCO (i.e. recur) with tail-position error checking
- Functions to debug
- Lazy sequences `lazy-cons` ???
- Lazy evaluation ???
- Move List to use proper linked list implementation
- REPL readline input only if parentesis are fully closed

## Features

- A great `analyzer` which find errors and missing definitions inside the running REPL before you eval your code
- Condition system with REPL block (enable/disable) and RESTARTS
- `ignore-errors` from CL
- Lazy evaluation ???
- Fast lists structures
- Immutable data structures
- Better stacktrace and errors
  - error show on line like rust
- Static typing?? - if it's worth it
- Datalog and an easy way to express logic and formality
- Multiarity and variadic function

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
