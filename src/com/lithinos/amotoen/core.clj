;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.core)

(declare pegasus)
(defprotocol IPosition
    (psdebug [t] "")
    (clone [t] "")
    (g [t] "Returns pos") ; E.V.I.L.
    (s [t j] "Sets pos") ; E.V.I.L.
    (m [t] "Returns the 'c' then (inc pos)")
    (c [t] "The character at pos"))
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
                (g [t] @j)
                (s [t k] (dosync (ref-set j k)))
                (clone [t] (gen-ps s @j))
                (m [t] (let [r (c t)] (dosync (alter j inc)) r))
                (c [t] (.charAt s @j))))))
(defn lpegs [t s] (reverse (into '() (cons t (seq s))))) ; This doesn't need to be fast, but shouldn't this work: (list (cons t (seq s)))
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

(defn- either [n g w]
    (let [original (g w)]
        (first (keep  #(do  (s w original)
                            (debug w "Either trying:" (pr-str %))
                            (pegasus % g w))
                        (rest n))))

(defn- type-list [n g w]
    (let [t (first n)
          b (second n)
          result (cond
                    (= t '|) (let [temp (either n g w)] (debug w "Either returning:" temp) temp)
                    (= t '*) (list
                                (doall
                                    (take-while
                                        #(cond
                                            (keyword? b)(b %)
                                            (list? b)   (do (debug w "Zero-or-more on list:" %) %)
                                            (char? b)   %)
                                        (repeatedly #(pegasus b g w)))))
                    (= t '?) (pegasus b g w)
                    (= t '%) (let [c    (c w)
                                   temp (pegasus b g w)]
                                ; If we succeed, then we fail - that's the point of AnyNot... If we fail, then we accept the current char
                                (if temp nil (do (m w) c) )))]
        result))

(defn- try-char [n w]
    (if (= n (c w))
        (do
            (debug w (str "MATCH: '" (pr-str n) "' with '" (c w) "'"))
            (m w))
        (do
            (debug w (str "Char mismatch: '" (pr-str n) "' with '" (c w) "'"))
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

