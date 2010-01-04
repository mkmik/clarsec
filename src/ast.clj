(ns ast)

(defn make-number-lit [n]
  {:type 'Lit :value n})

(defn make-string-lit [n]
  {:type 'Lit :value n})

(defn make-reference [n]
  {:type 'Ref :name n})

(defn make-struct-def [l v]
  {:type 'StructDef :label l :value v})

(defn make-instantiation [set args]
  {:type 'Inst :set set :args args})