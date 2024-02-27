# Clojure
## Namespace refers
- From a namespace A I `refer` another namespace B with a symbol `(def x 2)` defined into it, then I do `(ns-unmap x)` in the other namespace, the symbol is no more resolvable in B, but kept the same value and referring from A. So A keeps a copy of the symbol-value REPL says #object[clojure.lang.Var$Unbound 0x614aeccc "Unbound: #'B/x"] . The if I define again `(def x 3)` inside B, I will get two different values. In A `B/x` is valued `2`, then in B, `B/x` is valued `3`.
