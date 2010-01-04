(ns clarsec
  (:gen-class)
  (:use [de.kotka.monad]))


(defn consumed? [x]  (= (x :type) :consumed)) 
(defn failed? [x]  (= (x :type) :failed)) 

(defn failed [] {:type :failed})
(defn consumed [value rest] {:type :consumed
			     :value value
			     :rest rest})

(defn failback [v f] (if (nil? v) f v))


(declare Parser)

(derive 'clarsec/Parser 'de.kotka.monad/Monad)


(defmethod return 'Parser
  [t x]
  (make-monad t (fn p-return [strn] (consumed x strn))))
 

(defmethod bind 'Parser
  [m func] 
  (make-monad (monad-type m) 
	      (fn [strn] 
		(let [parser (monad m)
		      result (parser strn)]		  
		  (if (consumed? result)
		    ((monad (func (:value result))) (:rest result))
		    result
		    )
		  )
		)))

(defn result [v] (return 'Parser v))


(defn <|> [& parsers]
  (make-monad 'Parser
	      (fn opt-plus [strn]
		(failback
		 (first
		  (drop-while failed?
			      (map #((monad %) strn) parsers)))
		 (failed)
		 ))))

(defn >> [p1 p2]
  (bind p1 (fn [_] p2)))

(def either <|>)

; bind with a non monadic function
(defn >>== [p f]
  (bind p #(result (f %))))
;;

(def anyToken
  (make-monad 'Parser
	      (fn p-anyToken [strn]
		(if (= "" strn)
		  (failed)
		  (consumed (first strn)
			    (. strn (substring 1))))
		)
	      ))


(def fail
     (make-monad 'Parser (fn p-fail [strn] (failed))))

(defn satisfy [pred]
     (let-bind [c anyToken]
	       (if (pred c) (result c) fail)
     ))

(defn is-char [c]
  (satisfy (partial = c)))

(defn not-char [c]
  (satisfy #(not (= c %))))

(defn optional [p]
  (<|> p (result nil)))

;(defn string [strn]
;  (let-bind [x (m-sequence (map is-char strn))]
;	    (result (apply str x))))

(defn string [strn]
  (>>== (m-sequence (map is-char strn))
	 #(apply str %)))

(def many1)

(defn many [parser]
      (>>== (optional (many1 parser))
	    #(if (nil? %) () %)))

(defn many1 [parser]
      (let-bind
               [a parser
                as (many parser)]
               (result (concat [a] as))))

(defn endByM [f p sep]
  (f (let-bind [r p
		_ sep]
	       (result r))))

(defn endBy [p sep]
  (endByM many p sep))

(defn endBy1 [p sep]
  (endByM many1 p sep))

(defn sepBy1 [p sep]
  (let-bind [x p
	     xs (many (>> sep p))]
	    (result (cons x xs))))

(defn sepBy [p sep]
  (either (sepBy1 p sep) (result ())))

(defn followedBy [p sep]
  (let-bind [r p
	     _ sep]
	    (result r)))


(def letter
  (satisfy #(. Character isLetter %)))

(def digit
  (satisfy #(. Character isDigit %)))

(defn one-of [target-strn]
  (let [str-chars (into #{} target-strn)]
    (satisfy #(contains? str-chars %))))

(def space
     (one-of " \n\t"))

(def spaces (many space))

(defn lexeme [p]
  (>> spaces p))

(defn symb [name]
  (lexeme (string name)))

(def semi (symb ";"))
(def comma (symb ","))

; convert the result of a parse to a string, if its a list then concatenates the list
(defn stringify [p]
  (bind p #(result (if (seq? %) (apply str %) (str %)))))

(def identifier
  (lexeme (let-bind [c  letter
		     cs (many (either letter digit))]
		    (result (apply str (cons c cs))))))

(def natural
     (lexeme (>>== (stringify (many digit))
		   #(new Integer %))))


(defn between [open close p]
  (let-bind [_ open
	     x p
	     _ close]
	    (result x)))

(def stringLiteral
     (stringify (lexeme (between (is-char \") (is-char \") (many (not-char \"))))))

(defn parse [parser input] 
  ((monad parser) input)
)


;(defn -main []
;  (println (mytest "ciao mondomondo")))