;; Generic interface for functors

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
      :doc "Generic functor interface (fmap)"}
    nilern.cljs.algo.generic.functor)

(defmulti fmap
          "Applies function f to each item in the data structure s and returns
           a structure of the same kind."
          {:arglists '([f s])}
          (fn [f s] (type s)))

(defmethod fmap IList
           [f v]
           (map f v))

(defmethod fmap IVector
           [f v]
           (into (empty v) (map f v)))

(defmethod fmap IMap
           [f m]
           (into (empty m) (for [[k v] m] [k (f v)])))

(defmethod fmap ISet
           [f s]
           (into (empty s) (map f s)))

(defmethod fmap IFn
           [f fn]
           (comp f fn))

(prefer-method fmap IVector IFn)
(prefer-method fmap IMap IFn)
(prefer-method fmap ISet IFn)

(defmethod fmap LazySeq
           [f s]
           (map f s))

(defmethod fmap Delay
           [f d]
           (delay (f @d)))

(defmethod fmap nil
           [_ _]
           nil)