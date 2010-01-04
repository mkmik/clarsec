(ns doroty
  (:gen-class)
  (:use [clarsec]
	[ast]
	[de.kotka.monad])
)


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
     (sepBy expression comma))

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


(def predecl
     (string "undef"))

(def tagname (either (symb ".")
		     (let-bind [attr (option "" (string "@"))
				name (either identifier (symb "*"))]
			       (result (str attr name)))))

(def binaryPredicate
     (let-bind [xp xpath
		op (symb "=")
		expr (either (>>== expression make-xpath-expression) xpath)]
	       (result (make-binary-predicate op xp expr))))

(def predicate (either binaryPredicate (>>== xpath make-simple-predicate)))

(def tagexp
     (let-bind [axis (optional (followedBy identifier (symb "::")))
		tag  tagname
		pred (optional (brackets predicate))]
	       (result (make-tagexp axis tag pred))))

(def xpath
     (>>== (sepBy tagexp (string "/"))
	   make-xpath)
)

(def fieldList
     (sepBy identifier comma))

(def select
     (let-bind [_      (symb "select")
		fields (option [] (parens fieldList))
		xp     xpath]
	       (result (make-select fields xp))))


(def expression
     (either instantiation invocation literal))


(def statement 
     (either predecl select expression))

(def body 
     (endBy statement semi))

(def source
     (followedBy body (lexeme eof)))
