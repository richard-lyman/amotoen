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

(defprotocol IAmotoen
    (gp         [t]     "Get pos")
    (sp         [t j]   "Set pos")
    (end        [t]     "End of input")
    (c          [t]     "The character at pos")
    (m          [t]     "Returns the 'c' then (inc pos)"))

(defn wrap-string ; 'ps' is for a 'p'eggable 's'tring
    ([#^String s] (wrap-string s 0))
    ([#^String s j] ; 'j' is where we currently are in the input... 'i' was skipped since it was too close to 'i'nput
        (let [j (ref j)]
            (reify IAmotoen
                (gp     [t]     @j)
                (sp     [t k]   (dosync (ref-set j k)))
                (end    [t]     (= @j (count s)))
                (c      [t]     (try (.charAt s @j) (catch Exception e nil)))
                (m      [t]     (let [r (c t)]
                                    (when (nil? r) (throw (Exception. "Consuming nil")))
                                    (dosync (alter j inc))
                                    r))))))

(defn lpegs [t s] (reverse (into '() (cons t (seq s)))))
(defn pegs [s] (vec (seq s)))

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
    (let [original (gp w)]
        (first
            (keep
                #(do
                    (sp w original)
                    (pegasus % g w))
                (rest n)))))

(defn- any-not [b g w]
    (let [c (c w) p (gp w)]
        (if (pegasus b g w)
            (do (sp w p) nil) ; If we succeed, then we fail - that's the point of AnyNot... and rollback
            (do (m w) c)))); If we fail, then we accept the current char

(defn- try-char [n w]
    (if (= n (c w))
        (m w)
        nil))

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

(defn- zero-or-more [b g w]
    (doall
        (take-while
            #(if (keyword? b)
                (b %)
                %)
            (repeatedly #(pegasus b g w)))))

(defn- list-of-one-element [r]
    (and
        (seq? r)
        (nil? (seq (rest r)))
        (not (nil? (first r)))))

(defn- typed-list [n g w]
    (let [t (first n)
          b (second n)
          result (cond  (= t '|)    (either n g w)
                        (= t '%)    (any-not b g w)
                        (= t '*)    (zero-or-more b g w)
                        (= t 'a)    (b g w (pegasus (first (rest (rest n))) g w))
                        (= t 'f)    (b     (pegasus (first (rest (rest n))) g w)))]
        (if (list-of-one-element result)
            (first result)
            result)))

; If the rule and current position pair have already been seen...
(defn pegasus [n g w]
    (cond
        (keyword? n)(do (when (nil? (n g)) (throw (Error. (str "Keyword '" n "' does not exist in grammar"))))
                        (let [temp (pegasus (n g) g w)]
                            (if temp
                                {n temp}
                                nil)))
        (vector? n) (peg-vec n g w)
        (list? n)   (typed-list n g w)
        (char? n)   (try-char n w)
        true        (throw (Error. (str "Unknown type: " n)))))

(defn validate [g]
    (let [w     (wrap-string (pr-str g))
          ast   (pegasus :Grammar grammar-grammar w)
          r     (and    (not (nil? ast))
                        (end w))]
        [r, ast]))
(defn fastest-theoretical [s]
    (let [w (wrap-string s)]
        (loop [continue (not (end w))]
            (when continue
                (m w)
                (recur (not (end w)))))))
(defn self-check-fastest [] (fastest-theoretical (pr-str grammar-grammar)))
(defn self-check [] (validate grammar-grammar))
(defn self-ast []
    (pr-str (pegasus
        :Grammar
        grammar-grammar
        (wrap-string (pr-str grammar-grammar)))))

