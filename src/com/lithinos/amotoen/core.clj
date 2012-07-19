;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.core)

; s - input string
; w - input wrapper
; g - given grammar
; r - result to return
; n - next character in input

(declare pegasus)

(def ^:dynamic *currentK* (ref nil))

(defprotocol IPosition
    (psdebug    [t]     "Some form of helpful debug info")
    (in         [t]     "Indent - for debugging")
    (de         [t]     "Dedent - for debugging")
    (clone      [t]     "")
    (gp         [t]     "Get pos") ; E.V.I.L. ... maybe
    (sp         [t j]   "Set pos") ; E.V.I.L. ... maybe
    (end        [t]     "End of input")
    (m          [t]     "Returns the 'c' then (inc pos)")
    (c          [t]     "The character at pos"))

(defn gen-ps ; 'ps' is for a 'p'eggable 's'tring
    ([#^String s] (gen-ps s 0))
    ([#^String s j] ; 'j' is where we currently are in the input... 'i' was skipped since it was too close to 'i'nput
        (let [j (ref j)
              indent (ref 0)]
            (reify IPosition
                (psdebug [t]
                    (let [indent-string (apply str (take @indent (repeat "  ")))
                          padding (apply str (take 60 (repeat " ")))
                          before_j  (str "'" (pr-str (try
                                                        (subs s
                                                            (max 0 (- @j 30))
                                                            (max 0 @j))
                                                        (catch Exception e ""))) "'")
                          at_j      (str " " (pr-str (c t)) " ")
                          after_j   (str "'" (pr-str (try
                                                        (subs s
                                                            (inc @j)
                                                            (min (+ @j 30) (count s)))
                                                        (catch Exception e ""))) "'")]
                        (str (subs
                                (str (if (< @j 0)
                                        (str "<-" (subs s 0 20))
                                        (str before_j at_j after_j))
                                    padding)
                                0
                                60)
                            indent-string)))
                (in [t] (dosync (alter indent inc)))
                (de [t] (dosync (alter indent dec)))
                (gp [t] @j)
                (sp [t k] (dosync (ref-set j k)))
                (clone [t] (gen-ps s @j))
                (end [t] (= @j (count s)))
                (m [t] (let [r (c t)] (dosync (alter j inc)) r))
                (c [t] (try (.charAt s @j) (catch Exception e nil)))))))

(defn lpegs [t s] (reverse (into '() (cons t (seq s))))) ; This doesn't need to be fast, but shouldn't the following work? (list (cons t (seq s)))
(defn pegs [s] (vec (seq s)))

(def ^:dynamic *debug* (ref false))
(defn- debug [w & args]
    (when @*debug*
        (print (psdebug w))
        (print @*currentK* " ")
        (apply println args)
        (flush)))

(def #^{:private true} grammar-grammar {
    :_*             '(* :Whitespace)
    :_              [:Whitespace '(* :Whitespace)]
    :Grammar        [\{ :_* :Rule '(* [:_ :Rule]) :_* \}]
    :Rule           [:Keyword :_ :Body]
    :Keyword        [\: :AmotoenSymbol]
    :Body           '(| :Keyword :Char :Grouping :AnyNot :AwareFunction :Function)
    :Grouping       '(| :Sequence :Either :ZeroOrMore)
    :Sequence       [\[                 :_* :Body '(* [:_* :Body])  :_* \]]
    :Either         [\( \|              :_  :Body '(* [:_* :Body])  :_* \)]
    :ZeroOrMore     [\( \*              :_  :Body                   :_* \)]
    :AnyNot         [\( \%              :_  :Body                   :_* \)]
    :AwareFunction  [\( \a :_ :Symbol   :_  :Body                   :_* \)]
    :Function       [\( \f :_ :Symbol   :_  :Body                   :_* \)]
    :Whitespace                 '(| \space \newline \tab \,)
    :Char                       [\\ (list '| (pegs "tab") (pegs "space") (pegs "newline") '(% \space))]
    :Symbol                     '(| \/ :AmotoenSymbol)
    :AmotoenSymbol              [:NonNumericCharacter '(* :AlphanumericCharactersPlus)] ; _Not_ the same as a Clojure Symbol
    :NonNumericCharacter        (list '% (lpegs '| "0123456789"))
    :AlphanumericCharactersPlus (lpegs '| "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789:/*+!-_?.")
})

(defn- either [n g w]
    #_(let [original (gp w)]
        (loop [remaining (rest n)]
            (if (nil? (seq remaining))
                nil
                (do
                    (sp w original)
                    (let [result (pegasus (first remaining) g w)]
                        (if (nil? result)
                            (recur (rest remaining))
                            result))))))
    (let [original (gp w)] ; Why is this still the fastest?
        (first
            (keep
                #(do
                    (sp w original)
                    (pegasus % g w))
                (rest n))))
    #_(let [original (gp w)]
        #_(println "Processing either:" (first n) (rest n))
        (first
            (filter #(do #_(println "checking for nil:" %) (not (nil? %)))
                (doall
                    (pmap
                        #(do #_(println "Running with:" %) (pegasus % g (clone w)))
                        (rest n))))))
)

(defn- any-not [b g w]
    (let [c (c w) p (gp w)]
        (if (pegasus b g w)
            (do (sp w p) nil) ; If we succeed, then we fail - that's the point of AnyNot... and rollback
            (do
                #_(debug w "AnyNot MATCH:" (pr-str b) c)
                (m w)
                c)))); If we fail, then we accept the current char

(defn- try-char [n w]
    (if (= n (c w))
        (do
            #_(debug w (str "MATCH: '" (pr-str n) "' with '" (pr-str (c w)) "'"))
            (m w))
        (do
            #_(debug w (str "FAIL: '" (pr-str n) "' with '" (pr-str (c w)) "'"))
            nil)))

(defn- peg-vec [n g w]
    (let [p (gp w)]
        (loop [remaining    n
               result       []]
            (if (empty? remaining)
                result
                (let [temp (pegasus (first remaining) g w)]
                    (if temp
                        (recur (rest remaining)
                            (conj result temp))
                        (do
                            (sp w p)
                            nil)))))))

(defn- typed-list [n g w]
    (let [t (first n)
          b (second n)
          result (cond  (= t '|)    (let [temp (either n g w)]
                                        #_(debug w "Either returning:" (pr-str temp))
                                        temp)
                        (= t '%)    (any-not b g w)
                        (= t '*)    (doall (take-while #(if (keyword? b)
                                                            (b %)
                                                            %)
                                                        (repeatedly #(pegasus b g w))))
                        (= t 'a)    (b g w (pegasus (first (rest (rest n))) g w))
                        (= t 'f)    (b     (pegasus (first (rest (rest n))) g w)))]
        (if (and
                (seq? result)
                (nil? (seq (rest result)))
                (not (nil? (first result)))
                )
            (first result)
            result)))

(defn- p [w s n] #_(debug w s (pr-str n)))
(defn- fp [w s n]
    (dosync (ref-set *debug* true))
    (p w "c:" n)
    (dosync (ref-set *debug* false)))

(defn pegasus [n g w]
    (in w)
    (when (keyword? n) (dosync (ref-set *currentK* n)))
    (let [result (cond
                    (keyword? n)(do (p w "k:" n)
                                    (when (nil? (n g)) (throw (Error. (str "Keyword '" n "' does not exist in grammar"))))
                                    (let [temp (pegasus (n g) g w)]
                                        (if temp
                                            {n temp}
                                            nil)))
                    (vector? n) (do #_(p w "v:" n) (peg-vec n g w))
                    (list? n)   (do #_(p w "l:" n) (typed-list n g w))
                    (char? n)   (do #_(p w "c:" n) (try-char n w))
                    true        (throw (Error. (str "Unknown type: " n))))]
        (when (keyword? n) (dosync (ref-set *currentK* n)))
        (de w)
        result))

(defn validate
    ([g] (validate g false))
    ([g d]
        (dosync (ref-set *debug* d))
        (let [w (gen-ps (pr-str g))
              temp (pegasus :Grammar grammar-grammar w)
              r (or (nil? temp)
                    (not (end w)))]
            (dosync (ref-set *debug* false))
            [r, temp])))

(defn self-check [] (validate grammar-grammar))
(defn self-ast []
    (dosync (ref-set *debug* false))
    (let [r (pr-str (pegasus
                        :Grammar
                        grammar-grammar
                        (gen-ps (pr-str grammar-grammar))))]
        (dosync (ref-set *debug* false))
        r)
    )

