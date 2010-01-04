(ns eu.dnetlib.dlms.parser
  (:gen-class
   :name eu.dnetlib.dlms.ClojureDQLParser
   :implements [eu.dnetlib.dlms.jdbc.parser.IDQLParser]
   )
  (:use [eu.dnetlib.clojure.clarsec]
	[eu.dnetlib.dlms.ast]
	[eu.dnetlib.clojure.monad])
)

(declare instantiation invocation literal)
(declare xpath)


(def expression
     (delay (either instantiation invocation literal)))

(def stringLit
     (>>== stringLiteral make-string-lit))

(def number
     (>>== natural make-number-lit))

(def reference 
     (>>== identifier make-reference))

(def structureDef
     (let-bind [label identifier
		_     (symb "=")
		val   expression]
	       (result (make-struct-def label val))))

(def structure 
     (brackets (sepBy structureDef comma)))

(def literal
     (either structure number stringLit reference))
     
(def argList
     (delay (sepBy expression comma)))

(def instantiation
     (let-bind [_      (symb "new")
		set    identifier
		args   (parens argList)]
	       (result (make-instantiation set args))))

(def invocation
     (let-bind [target identifier
		_      (string ".")
		method identifier
		args  (parens argList)]
		(result (make-call target method args))))


(defn decl [typ]
  (let-bind [name identifier
	     _    (symb "=")
	     e    expression]
	    (result (make-decl-init typ name e))))

(defn assign [name]
  (let-bind [_ (symb "=")
	     e expression]
	    (result (make-assign name e))))

(def predecl
     (let-bind [name identifier]
	       (either (decl name) (assign name))))



(def tagname (either (symb ".")
		     (let-bind [attr (option "" (string "@"))
				name (either identifier (symb "*"))]
			       (result (str attr name)))))


(def binaryPredicate
     (delay
      (let-bind [xp xpath
		 op (symb "=")
		 expr (either (>>== expression make-xpath-expression) xpath)]
		(result (make-binary-predicate op xp expr)))))

(def predicate (delay (either binaryPredicate (>>== xpath make-simple-predicate))))

(def tagexp
     (delay
      (let-bind [axis (optional (followedBy identifier (symb "::")))
		 tag  tagname
		 pred (optional (brackets predicate))]
		(result (make-tagexp axis tag pred)))))


(def xpath
     (delay
      (>>== (sepBy tagexp (symb "/"))
	    make-xpath)))


(def fieldList
     (sepBy identifier comma))

(def select
     (let-bind [_      (symb "select")
		fields (option [] (parens fieldList))
		xp     xpath]
	       (result (make-select fields xp))))

(def statement 
     (either predecl select expression))

(def body 
     (followedBy (sepBy1 statement semi) (optional semi)))

(def source
     (followedBy body (lexeme eof)))

(defn -main []
  (println (parse source "1")))

(defn -parse [strn]
  (:value (parse source strn)))
