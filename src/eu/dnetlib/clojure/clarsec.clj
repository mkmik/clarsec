(ns eu.dnetlib.clojure.clarsec
  (:gen-class)
  (:use [eu.dnetlib.clojure.monad]))


(defn consumed? [x]
  (= (x :type) :consumed))

(defn failed? [x]
  (= (x :type) :failed))

(defn failed []
  {:type :failed})

(defn consumed [value rest]
  {:type :consumed
   :value value
   :rest rest})

(defn failback [v f] (if (nil? v) f v))


(declare Parser)

(derive 'clarsec/Parser 'de.kotka.monad/Monad)


(defmethod return 'Parser
  [t x]
  (make-monad t (fn p-return [strn] (consumed x strn))))

(defmethod bind 'Parser
  [dm dfunc]
  (let [m (force dm)
        func (force dfunc)]
    (make-monad (monad-type m)
                (fn [strn]
                  (let [parser (monad m)
                        result (parser strn)]
                    (if (consumed? result)
                      ((force (monad (force (func (:value result))))) (:rest result))
                      result))))))

(defn result [v]
  (return 'Parser v))

(defn <|> [& parsers]
  (make-monad 'Parser
              (fn opt-plus [strn]
                (failback
                 (first (drop-while failed? (map #((monad (force %)) strn) parsers)))
                 (failed)))))

(defn >> [p1 p2]
  (bind p1 (fn [_] p2)))

(def either <|>)

;; Bind with a non monadic function
(defn >>== [p f]
  (bind p #(result (f %))))

(def any-token
     (make-monad 'Parser
                 (fn p-any-token [strn]
                   (if (= "" strn)
                     (failed)
                     (consumed (first strn)
                               (. strn (substring 1)))))))

(def eof
     (make-monad 'Parser
                 (fn p-eof [strn]
                   (if (= "" strn)
                     (consumed "" "")
                     (failed)))))

(def fail (make-monad 'Parser (fn p-fail [strn] (failed))))

(defn satisfy [pred]
  (let-bind [c any-token]
            (if (pred c) (result c) fail)))

(defn is-char [c]
  (satisfy (partial = c)))

(defn not-char [c]
  (satisfy #(not (= c %))))

(defn optional [p]
  (<|> p (result nil)))

(defn option [default p]
  (<|> p (result default)))


(defn string [strn]
  (>>== (m-sequence (map is-char strn))
        #(apply str %)))

(def many1)

(defn many [parser]
  (>>== (optional (many1 parser))
        #(if (nil? %) () %)))

(defn many1 [parser]
  (let-bind [a parser
             as (many parser)]
    (result (concat [a] as))))

(defn end-by-m [f p sep]
  (f (let-bind [r p
                _ sep]
       (result r))))

(defn end-by [p sep]
  (end-by-m many p sep))

(defn end-by-1 [p sep]
  (end-by-m many1 p sep))

(defn sep-by-1 [p sep]
  (let-bind [x p
             xs (many (>> sep p))]
            (result (cons x xs))))

(defn sep-by [p sep]
  (either (sep-by-1 p sep) (result ())))

(defn followed-by [p sep]
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

(def space (one-of " \r\n\t"))

(def spaces (many space))

(defn lexeme [p]
  (>> spaces p))

(defn symb [name]
  (lexeme (string name)))

(def semi (symb ";"))
(def comma (symb ","))

;; Convert the result of a parse to a string, if it's a list then concatenates the list...
(defn stringify [p]
  (bind p #(result (if (seq? %) (apply str %) (str %)))))

(def base-identifier
     (let-bind [c  letter
                cs (many (either letter digit))]
               (result (apply str (cons c cs)))))

(def identifier
     (lexeme base-identifier))

(def natural
     (lexeme (>>== (stringify (many1 digit))
                   #(new Integer %))))

(defn between [open close p]
  (let-bind [_ open
             x p
             _ close]
            (result x)))

(defn parens [p]
  (between (symb "(") (symb ")") p))

(defn brackets [p]
  (between (symb "[") (symb "]") p))

(defn braces [p]
  (between (symb "{") (symb "}") p))


(def stringLiteral
     (stringify (lexeme (between (is-char \") (is-char \") (many (not-char \"))))))

(defn parse [parser input]
  ((monad (force parser)) input))

;;(defn -main []
;;  (println (parse (>> (delay letter) (delay letter)) "ca.")))