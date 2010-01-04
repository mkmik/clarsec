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
		_ (symb "=")
		val expression]
	       (result (make-struct-def label val))))

(def structure 
     (brackets (sepBy structureDef comma)))

(def literal
     (either structure number stringLit reference))
     



(def predecl
     (string "undef"))

(def select
     (string "ugo"))

(def expression
     literal)


(def statement 
     (either predecl select expression))

(def body 
     (endBy statement semi))

(def source
     (followedBy body (lexeme eof)))
