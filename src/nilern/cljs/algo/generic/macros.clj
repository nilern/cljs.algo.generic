(ns nilern.cljs.algo.generic.macros)

;
; Macros to permit access to the / multimethod via namespace qualification
;
(defmacro defmethod*
  "Define a method implementation for the multimethod name in namespace ns.
   Required for implementing the division function from another namespace."
  [ns name & args]
  (let [qsym (symbol (str ns) (str name))]
    `(defmethod ~qsym ~@args)))

(defmacro qsym
  "Create the qualified symbol corresponding to sym in namespace ns.
   Required to access the division function from another namespace,
   e.g. as (qsym clojure.algo.generic.arithmetic /)."
  [ns sym]
  (symbol (str ns) (str sym)))

; This used to be in clojure.contrib.def (by Steve Gilardi),
; which has not been migrated to the new contrib collection.
(defmacro defmacro-
  "Same as defmacro but yields a private definition"
  [name & decls]
  (list* `defmacro (with-meta name (assoc (meta name) :private true)) decls))

; One-argument math functions
(defmacro- defmathfn-1
           [name]
           (let [js-symbol (symbol "js" (str "Math." name))]
             `(do
                (defmulti ~name
                          ~(str "Return the " name " of x.")
                          {:arglists '([~'x])}
                          type)
                (defmethod ~name js/Number
                           [~'x]
                  (~js-symbol ~'x)))))

; Two-argument math functions
(defmacro- defmathfn-2
           [name]
           (let [js-symbol (symbol "js" (str "Math." name))]
             `(do
                (defmulti ~name
                          ~(str "Return the " name " of x and y.")
                          {:arglists '([~'x ~'y])}
                          two-types)
                (defmethod ~name [js/Number js/Number]
                           [~'x ~'y]
                  (~js-symbol ~'x ~'y)))))
