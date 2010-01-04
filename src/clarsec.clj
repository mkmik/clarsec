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




(declare Parser)

(derive 'clarsec/Parser 'de.kotka.monad/Monad)


(defmethod return 'Parser
  [t x]
  (make-monad t (fn p-return [strn] (consumed x strn))))
 

(defmethod bind 'Parser
  [m func] 
;  (println "binding" m)
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

(def fail-char
     (make-monad 'Parser 
		 (fn p-fail-char [strn]
		   (failed))))
  

;(def myparser 
;     (let-bind [x (return 'Parser 12)] x)
;)

(defn parse [parser input] 
  ((monad parser) input)
)


;(defn -main []
;  (println (mytest "ciao mondomondo")))