(ns clarsec
  (:gen-class)
  (:use [monad]
	[de.kotka.monad]))


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

;;

(def any-char
  (make-monad 'Parser
	      (fn p-any-char [strn]
		(if (= "" strn)
		  (failed)
		  (consumed (first strn)
			    (. strn (substring 1))))
		)
	      ))


(def fail
     (make-monad 'Parser (fn p-fail [strn] (failed))))

(defn satisfy [pred]
     (let-bind [c any-char]
	       (if (pred c) (result c) fail)
     ))

(defn optional [p]
  (<|> p (result nil)))

(defn string [strn]
  (let-bind [x (m-sequence (map is-char strn))]
	    (result (apply str x))))

(def many1)

(defn many [parser]
      (let-bind [res (optional (many1 parser))]
	(println "MANY GOT" res)
	(result (if (nil? res) () res))))

(defn many1 [parser]
      (let-bind
               [a parser
                as (many parser)]
               (result (concat [a] as))))


(defn is-char [c]
  (satisfy (partial = c)))

(def letter
  (satisfy #(. Character isLetter %)))

(def digit
  (satisfy #(. Character isDigit %)))




;(def myparser 
;     (let-bind [x (return 'Parser 12)] x)
;)

(defn parse [parser input] 
  ((monad parser) input)
)


;(defn -main []
;  (println (mytest "ciao mondomondo")))