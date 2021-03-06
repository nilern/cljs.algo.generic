;; Support code for generic interfaces

;; by Konrad Hinsen

;; Copyright (c) Konrad Hinsen, 2009-2011. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns
  ^{:author "Konrad Hinsen, Pauli Jaakkola"
     :skip-wiki true
     :doc "Generic interfaces
           This library provides generic interfaces in the form of
           multimethods that can be implemented for any type.
           The interfaces partly duplicate existing non-generic
           functions in clojure.core (arithmetic, comparison,
           collections) and partly provide additional functions that
           can be defined for a wide variety of types (functors, math
           functions)."}
  nilern.cljs.algo.generic)

;
; A dispatch function that separates nulary, unary, binary, and
; higher arity calls and also selects on type for unary and binary
; calls.
;
(defn nary-dispatch
  ([] ::nulary)
  ([x] (type x))
  ([x y]
     [(type x) (type y)])
  ([x y & more] ::nary))

;
; We can't use [::binary :default], so we need to define a root type
; of the type hierarcy. The derivation for Object covers all classes,
; but all non-class types will need an explicit derive clause.
; Since non-class types are rare since Clojure 1.2, this need is
; not likely to be frequent.
;
(def root-type ::any)
(derive js/Object root-type)

;
; Symbols referring to ::nulary and ::n-ary
;
(def nulary-type ::nulary)
(def nary-type ::nary)
