;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.core)

(defn debug [& args] #_(apply println args))

;(defn lpegs [t s] (list (cons t (seq s)))) ; Somehow this doesn't work... (list? (list ...)) returns false...
(defn lpegs [t s] (reverse (into '() (cons t (seq s)))))
(defn pegs [s] (vec (seq s)))

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

(declare pegasus)

(defprotocol IPosition (clone [t] "") (m [t] "Returns the 'c' then (inc pos)") (c [t] "The character at pos"))
(defn gen-ps
    ([s] (gen-ps s 0))
    ([s j]
        (let [j (ref j)]
            (reify IPosition
                (clone [t] (gen-ps s @j))
                (m [t] (let [r (c t)] (dosync (alter j inc)) r))
                (c [t] (.charAt s @j))))))

(defn- either [n g w] (first (keep #(do (debug "Either trying:" %) (pegasus % g w)) (rest n))))

(defn- type-list [n g w]
    (let [t (first n)
          result (cond
                    (= t '|) (let [temp (either n g w)] (debug "Either returning:" temp) temp)
                    (= t '*) (list
                                (doall
                                    (take-while
                                        #(if (map? %)
                                            (do
                                                (debug "Filter * first:" ((second n) %) %)
                                                ((second n) %))
                                            (do
                                                (debug "Filter * second:" %) ; This can't be what it should be... this would fail the :Grammar (* [:_ :Rule]) bit
                                                false))
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
            (debug (str "MATCH: '" (pr-str n) "' with '" (c w) "'"))
            (m w))
        (do
            (debug (str "Char mismatch: '" (pr-str n) "' with '" (c w) "'"))
            nil)))

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

(defn- p [s n] (debug s (pr-str n)) (flush))

(defn pegasus [n g w]
    (cond
        (keyword? n)(do (p "k:" n) (flush) (let [temp (pegasus (n g) g w)] (if temp {n temp} nil)))
        (vector? n) (do (p "v:" n) (flush) (peg-vec n g w))
        (list? n)   (do (p "l:" n) (flush) (type-list n g w))
        (char? n)   (do #_(p "c:" n) (flush) (try-char n w))
        true        (throw (Error. (str "Unknown type: " n)))))

(defn self-check []
    #_(println (pr-str (pegasus :Grammar grammar-grammar (gen-ps (pr-str {:S \a}))))) (flush)
    #_(println (pr-str (pegasus :Grammar grammar-grammar (gen-ps "{:S \\a}")))) (flush)
    ; Shouldn't be nil
    (println (pr-str (pegasus :Grammar grammar-grammar (gen-ps (pr-str grammar-grammar))))) (flush)
    )

(self-check)

