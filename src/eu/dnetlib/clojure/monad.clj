;;-
;; Copyright 2008 (c) Meikel Brandmeyer.
;; All rights reserved.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;; this is a slightly patched version which accepts "Delay" objects is monadic objects
;; this hack shouldn't be necessary but I was in a hurry
;; the correct solution is to perform the delay forcing within the monadic operators

(ns eu.dnetlib.clojure.monad
  (:use [clojure.contrib.def :only (defstruct-)]))

(declare Monad MZero MRunnable)

(derive `MZero `Monad)

(defstruct- monad-structure :type :monad)

(defn make-monad
  "Bless an object with the given monad type."
  [t m]
  (struct monad-structure t m))

(defn monad
  "Return the monad of a blessed object."
  [m]
  (m :monad))

(defn monad-type
  "Return the type of a blessed object."
  [m]
  (m :type))

(defmulti
  #^{:arglists '([monad-type monad-argument])
     :doc "Return a monad of the given type with the given argument."}
  return
  (fn [t _] t))

(defmethod return `Monad
  [t m]
  (make-monad t m))

(defmulti
  #^{:doc "bind makes the value of the given monad available to a function.
  The function may act on the value, but it must return another monad.
  Although this cannot be enforced in Clojure."}
  bind (fn [m _] (monad-type (force m))))

(defmethod bind `MZero [m _] m)
(defmethod bind `Monad [m f] (f (monad m)))

(defmulti
  #^{:doc "If the first argument is not a `MZero, return it. Otherwise return the second value."}
  mplus
  (fn [m1 m2] #(vec (map monad-type [%1 %2]))))

(defmethod mplus [`MZero `Monad] [_ m2] m2)
(defmethod mplus [`Monad `Monad] [m1 _] m1)

(defmacro let-bind
  "let-bind binds the result of the given monads to the given variables
  and executes the body in an implicit do block. How this done exactly
  depends on the actual monad. The let-bind body should again return a
  monad."
  [clauses & body]
  (let [[v monad & clauses] clauses]
    (if (nil? clauses)
      `(bind ~monad (fn [~v] ~@body))
      `(bind ~monad (fn [~v] (let-bind ~clauses ~@body))))))

(defn m-sequence
  "Convert the given sequence of monads into a monad of the given
  type with the value set to the sequence of the values. There must
  be at least one monad in the monad collection."
  [monads]
  (let [f (fn [ms m]
            (let-bind [result  m
                       results ms]
                      (return (monad-type m) (conj results result))))]
    (reduce f (return (-> monads first monad-type) nil) (reverse monads))))

(defmacro lift-into
  "Call the given function f into the given monad with the values of
  all given monads in the same order."
  [m f & ms]
  (let [xs (take (count ms) (drop 1 (iterate gensym "lift__")))]
    `(let-bind ~(vec (interleave xs ms)) (return ~m (~f ~@xs)))))

(defmulti
  #^{:arglists '([monad & args])
     :doc "Applies the value of the given monad to the arguments and returns
  the result. Not all monad types are `Runnable."}
  run
  (fn [m & _] (monad-type m)))

(defmethod run `MRunnable
  [m & args]
  (apply (monad m) args))
