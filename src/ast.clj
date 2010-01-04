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

(defn make-call [target method args]
  {:type 'Call :target target :method method :args args})

(defn make-select [fields xp]
  {:type 'Select :fields fields :xpath xp})

(defn make-xpath [comps]
  {:type 'XPath :components comps})

(defn make-tagexp [axis tag pred]
  {:type 'XPathComponent :axis axis :tag tag :pred pred})

(defn make-simple-predicate [xp]
  {:type 'SimplePredicate :xpath xp})

(defn make-binary-predicate [op xp expr]
  {:type 'BinaryPredicate :op op :xpath xp :expression expr})

(defn make-xpath-expression [ex]
  {:type 'XPathPredicate :expression ex})

(defn make-decl-init [typ name e]
  {:type 'DeclInit :type typ :name name :expression e})

(defn make-assign [name e]
  {:type 'Assign :name name :expression e})
