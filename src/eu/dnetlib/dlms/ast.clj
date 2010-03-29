(ns eu.dnetlib.dlms.ast
  (:import (eu.dnetlib.dlms.jdbc.ast 
	    Assign BinaryPredicate Call DeclInit Expression Inst Lit NumberLit Predicate Ref RunExpr Select SimplePredicate StringLit Struct 
	    StructKeyValue XPathComponent XPathExpr XPath Parameter CollectionNode
	    )))
	    
(defn make-run-expr [e]
  (new RunExpr e))

(defn make-number-lit [n]
  (new NumberLit n))

(defn make-string-lit [n]
  (new StringLit n))

(defn make-reference [n]
  (new Ref n))

(defn make-parameter [n]
  (new Parameter n))

(defn make-collection [values]
  (new CollectionNode values))

(defn make-struct [defs]
  (new Struct defs))

(defn make-struct-def [l v]
  (new StructKeyValue l v))

(defn make-instantiation [set args]
  (new Inst set args))

(defn make-call [target method args]
  (new Call target method args))

(defn make-select [fields xp]
  (new Select fields xp))

(defn make-xpath [comps]
  (new XPath comps))

(defn make-tagexp [axis tag pred]
  (new XPathComponent tag pred axis))

(defn make-simple-predicate [xp]
  (new SimplePredicate xp))

(defn make-binary-predicate [op xp expr]
  (new BinaryPredicate op xp expr))

(defn make-xpath-expression [ex]
  (new XPathExpr ex))

(defn make-decl-init [typ name e]
  (new DeclInit typ name e))

(defn make-assign [name e]
  (new Assign name e))
