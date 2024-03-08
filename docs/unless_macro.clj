(defmacro unless [pred a b]
  `(if (not ~pred) ~a ~b))

;; (list 'do
;;       (cons `defn decl)
;;       (list '. (list 'var name) '(setMacro))
;;       (list 'var name))

(do
  (defn unless
    ([&form &env pred a b]
     (seq
      (concat (list (quote if))
              (list
               (seq (concat (list (quote not))
                            (list pred))))
              (list a)
              (list b)))))
  (. (var unless) (setMacro))
  (var unless))

(do
  (defn unless
    ([&form &env pred a b]
     (seq
      (concat ('if)
              (list
               (seq (concat ('not)
                            (pred))))
              (a)
              (b)))))
  (. (var unless) (setMacro))
  (var unless))

(let [ret (clojure.lang.RT/assoc map key val)]
  (if kvs
    (if (next kvs)
      (recur ret (first kvs) (second kvs) (nnext kvs))
      (throw (IllegalArgumentException.
              "assoc expects even number of arguments after map/vector, found odd number")))
    ret))

(let* [ret (clojure.lang.RT/assoc map key val)]
      (if kvs
        (if (next kvs)
          (recur ret (first kvs) (second kvs) (nnext kvs))
          (throw (IllegalArgumentException. "assoc expects even number of arguments after map/vector, found odd number")))
        ret))
