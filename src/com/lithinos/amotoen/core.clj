;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.core)

(declare pegasus)
(defprotocol IPosition (psdebug [t] "") (clone [t] "") (m [t] "Returns the 'c' then (inc pos)") (c [t] "The character at pos"))
(defn gen-ps
    ([s] (gen-ps s 0))
    ([s j]
        (let [j (ref j)]
            (reify IPosition
                (psdebug [t]
                    (if (< @j 0)
                        (str "<-" (subs s 0 20))
                        (str    "'" (subs s (max 0 (- @j 30)) (max 0 @j)) "'"
                                " ->" (c t) "<- "
                                "'" (subs s (inc @j) (min (+ @j 30) (count s))) "'")))
                (clone [t] (gen-ps s @j))
                (m [t] (let [r (c t)] (dosync (alter j inc)) r))
                (c [t] (.charAt s @j))))))
;(defn lpegs [t s] (list (cons t (seq s)))) ; Somehow this doesn't work... (list? (list ...)) returns false...
(defn lpegs [t s] (reverse (into '() (cons t (seq s))))) ; ... but this doesn't need to be fast
(defn pegs [s] (vec (seq s)))


(defn- debug [w & args] (print (psdebug w)) (apply println args))
(defn- debugt [w & args] (print (psdebug w)) (apply println args))

(def #^{:private true} grammar-grammar {
    :Whitespace     '(| \space \newline \tab)
    :_*             '(* :Whitespace)
    :_              [:Whitespace '(* :Whitespace)]
    :Grammar        [\{ :_* :Rule '(* [:_ :Rule]) :_* \}]
    :Rule           [:Keyword :_ :Body]
    :Keyword        [\: :ValidKeywordChar '(* :ValidKeywordChar)]
    :Body           '(| :Keyword :Grouping :Char)
    :Grouping       '(| :Sequence :Either :ZeroOrMore :ZeroOrOne :AnyNot)
    :Sequence       [\[         :_* [:Body '(* [:_* :Body])]    :_* \]]
    :Either         [[\( \|]    :_  [:Body '(* [:_* :Body])]    :_* \)]
    :ZeroOrMore     [[\( \*]    :_  :Body                       :_* \)]
    :ZeroOrOne      [[\( \?]    :_  :Body                       :_* \)]
    :AnyNot         [[\( \%]    :_  '(| :Keyword :Char)         :_* \)]
    :Char           [\\ '(| :TabChar :SpaceChar :NewlineChar (% \space)) '(* \space)]
    :TabChar        (pegs "tab")
    :SpaceChar      (pegs "space")
    :NewlineChar    (pegs "newline")
    :ValidKeywordChar (lpegs '| "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789:/*+!_?-")
})

(defn- either [n g w] (first (keep #(do (debug w "Either trying:" (pr-str %)) (pegasus % g w)) (rest n))))

(defn- type-list [n g w]
    (let [t (first n)
          result (cond
                    (= t '|) (let [temp (either n g w)] (debug w "Either returning:" temp) temp)
                    (= t '*) (list
                                (doall
                                    (take-while
                                        #(if (map? %)
                                            (do
                                                (debugt w "Filter * first:" ((second n) %) %)
                                                ((second n) %))
                                            (do
                                                (debugt w "Filter * second:" % "," n "," (second n)) ; This can't be what it should be... this would fail the :Grammar (* [:_ :Rule]) bit
                                                ;(not (nil? %))
                                                (if (nil? %)
                                                    nil
                                                    true)
                                                ))
                                        (repeatedly #(pegasus (second n) g w)))))
                    (= t '?) (pegasus (second n) g w)
                    (= t '%) (let [c    (c w)
                                   temp (pegasus (second n) g w)]
                                (if temp ; If we succeed, then we fail - that's the point of AnyNot
                                    nil
                                    (do (m w) c) ; If we fail, then we accept the current char
                                    )))]
        result))

(defn- try-char [n w]
    (if (= n (c w))
        (do
            (debug w (str "MATCH: '" (pr-str n) "' with '" (c w) "'"))
            (m w))
        (do
            (debug w (str "Char mismatch: '" (pr-str n) "' with '" (c w) "'"))
            nil)))

; Failure needs to move the pos back...
(defn- peg-vec [n g w]
    (loop [remaining    n
           result       []]
        (if (empty? remaining)
            result
            (let [temp (pegasus (first remaining) g w)]
                (if temp
                    (recur  (rest remaining)
                            (conj result temp))
                    nil)))))

(defn- p [w s n] (debug w s (pr-str n)) (flush))

(defn pegasus [n g w]
    (cond
        (keyword? n)(do (p w "k:" n) (flush) (let [temp (pegasus (n g) g w)] (if temp {n temp} nil)))
        (vector? n) (do (p w "v:" n) (flush) (peg-vec n g w))
        (list? n)   (do (p w "l:" n) (flush) (type-list n g w))
        (char? n)   (do #_(p w "c:" n) (flush) (try-char n w))
        true        (throw (Error. (str "Unknown type: " n)))))

(defn self-check []
    #_(println (pr-str (pegasus :Grammar grammar-grammar (gen-ps (pr-str {:S \a}))))) (flush)
    #_(println (pr-str (pegasus :Grammar grammar-grammar (gen-ps "{:S \\a}")))) (flush)
    ; Shouldn't be nil
    (println (pr-str (pegasus :Grammar grammar-grammar (gen-ps (pr-str grammar-grammar))))) (flush)
    )

(self-check)

