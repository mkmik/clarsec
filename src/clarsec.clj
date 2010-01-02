(ns clarsec
  (:gen-class)
  (:use [clojure.contrib.monads]))

(defn consumed? [x]  (= (:type x) :consumed)) 
(defn failed? [x]  (= (:type x) :failed)) 

(defn failed [] {:type :failed})
(defn consumed [value rest] {:type :consumed
			     :value value
			     :rest rest})

(defn failback [v f] (if (nil? v) f v))

(defmonad parser-m 
  [m-result (fn m-result-parser [x] (fn [strn] (consumed x strn)))
   m-bind (fn m-bind-parser [parser func]
	    (fn [strn]
	      (let [result (parser strn)]
		(if (consumed? result)
		  ((func (:value result)) (:rest result))
		  result
		))))

   m-zero (fn [strn] (failed))
   
   m-plus (fn [& parsers]
	    (fn [strn]
	      (failback
	       (first
		(drop-while failed?
			    (map #(% strn) parsers)))
	       (failed)
	       )))
   
   ]
)

(defmonadfn any-char [strn]
  (if (= "" strn)
    (failed)
    (consumed (first strn)
	      (. strn (substring 1))))
  )

(defmonadfn is-char [c]
      (char-test (partial = c)))

(defmonadfn satisfy [pred]
  (domonad [c any-char
	    :when (pred c)]
	   (str c)))

(defmonadfn string [strn]
  (domonad [x (m-seq (map is-char strn))]
	   (apply str x))
)

(defmonadfn optional [parser]
  (m-plus parser (m-result (failed))))

;(defmonadfn <|> [& args] 
; (apply m-plus args))

(defmacro <|> [& args]
  (cons 'm-plus args)
)

(defmonadfn body [] 
  (domonad [x any-char
	    y any-char] (str y x))
)

(defmonadfn body2 [] 
  (domonad [x (<|> (string "ciao") (string "ugo"))]
	   x)
)


(defmacro parse [p i]
  (list 'with-monad 'parser-m (list (list p) i))
)



(defn mytest [n] 
  (parse #(string "ciao") n)
)

(defn -main []
  (println (mytest "ciao mondo"))
)