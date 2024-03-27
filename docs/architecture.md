## Main

- When _FMLisp_ starts, there are two execution options:
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

## Discussions

### Fn Traits and Lazy seq

LazySeq is a data structure that saves a sequence and a function and provide the interface for creating an iterator with it. So the returning value (a PersistentList) will be realized only when the function is executed on the list. This behaviour makes possible to create lists only at execution time.

How to implement a LazySeq? I can use a proper data structure to do that. The data structure receives a Lambda and a Sequence.

I can also delay the implementations details for later and for now work only without LazySeq.
https://clojure-goes-fast.com/blog/clojures-deadly-sin/

### Struct namespace

Clojure inherith and load automatically a lot of Java namespaces that contains Java code to be called with interop. Unfortunately, our language would not be able to have the level of interop that Clojure has. But I wonder if there's an easy way to load a file/struct easily in the environment, using a separated namespace, making it usable from FMLisp.

### Reader with line/col cursor for error

The reader actually uses only tokens, which is a vector of type
["(", "do", "(", "defn", "foo", "[", "x", "]", "(", "+", "x", "1", ")", ")"]

The feature I would like to add are:

- multi sexpr, currently achieved using a wrapping `(do ..)`. I want to replace it with a reader that support it with a variable, useful to distinguish between repl input and file reading.
- line/col

Solutions:

- Save the source in the Reader. When it raises an error, find row/col in the sexpr.

### Lisp Reader

https://clojure.org/reference/reader

The Lisp Reader convert the code rappresentation into data processable by the compiler.
The Reader automatically convert some macros, without waiting for the code to be evaluated. For example quote, deref, comment, dispatch, metadata, syntaxquote, unquote, unquote-splicing.
So the Reader should be able to process the AST and extend it directly before reach the _eval_ phase.
Also the `gensym` reader macro `sym#` get's extended at _read_ time.
