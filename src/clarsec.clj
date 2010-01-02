(ns clarsec
  (:gen-class)
  (:use [clojure.contrib.monads]))

(defn consumed? [x]  (= (:type x) :consumed)) 
(defn failed? [x]  (= (:type x) :failed)) 

(defn failed [] {:type :failed})
(defn consumed [value rest] {:type :consumed
			     :value value
			     :rest rest})

(defmonad parser-m 
  [m-result (fn m-result-parser [x] (fn [str] {:type :consumed
			       :value x
			       :rest str}))
   m-bind (fn m-bind-parser [parser func]
	    (fn [strn]
	      (let [result (parser strn)]
		(if (consumed? result)
		  ((func (:value result)) (:rest result))
		  result
		))))

   m-zero (fn [strn] (failed))
   
   m-plus (fn [& parsers] (println "ciao" parsers))
   ]
)

(defmonadfn any-char [strn]
      (if (= "" strn)
          (failed)
	  (consumed (first strn)
		    (. strn (substring 1))))
)


(defmonadfn body [] 
  (domonad [x any-char
	    y any-char] (str y x))
)

(defmonadfn body2 [] 
  (m-seq [any-char any-char any-char])
)


(defn mytest [n] 
  (with-monad parser-m ((body) n)
    ))

(defmacro parse [p i]
  (list 'with-monad 'parser-m (list (list p) i))
)


