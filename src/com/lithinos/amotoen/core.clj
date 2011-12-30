;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.core
    (:use clojure.pprint))

(declare pegasus)
(defprotocol IPosition
    (psdebug    [t]     "Some form of helpful debug info")
    (clone      [t]     "")
    (gp         [t]     "Get pos") ; E.V.I.L. ... maybe
    (sp         [t j]   "Set pos") ; E.V.I.L. ... maybe
    (in         [t]     "Indent - for debugging")
    (de         [t]     "Dedent - for debugging")
    (end        [t]     "End of input")
    (m          [t]     "Returns the 'c' then (inc pos)")
    (c          [t]     "The character at pos"))
(defn gen-ps
    ([s] (gen-ps s 0))
    ([s j]
        (let [j (ref j)
              indent (ref 0)]
            (reify IPosition
                (psdebug [t]
                    (let [indent-string (apply str (take @indent (repeat "  ")))
                          padding (apply str (take 60 (repeat " ")))
                          x (str "'" (pr-str (subs s (max 0 (- @j 30)) (max 0 @j))) "'")
                          y (str " " (pr-str (c t)) " ")
                          z (str "'" (pr-str (subs s (inc @j) (min (+ @j 30) (count s)))) "'")]
                        (str (subs (str (if (< @j 0) (str "<-" (subs s 0 20)) (str x y z)) padding) 0 60) indent-string)))
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
(defn- debug [w & args] (when @*debug* (print (psdebug w)) (apply println args) (flush)))

(def #^{:private true} grammar-grammar {
    :Whitespace     '(| \space \newline \tab \,)
    :_*             '(* :Whitespace)
    :_              [:Whitespace '(* :Whitespace)]
    :Grammar        [\{ :_* :Rule '(* [:_ :Rule]) :_* \}]
    :Rule           [:Keyword :_ :Body]
    :Keyword        [\: :ValidKeywordChar '(* :ValidKeywordChar)]
    :Body           '(| :Keyword :Char :Grouping)
    :Grouping       '(| :Sequence :Either :ZeroOrMore :ZeroOrOne :AnyNot)
    :Sequence       [\[     :_* :Body '(* [:_* :Body])  :_* \]]
    :Either         [\( \|  :_  :Body '(* [:_* :Body])  :_* \)]
    :ZeroOrMore     [\( \*  :_  :Body                   :_* \)]
    :ZeroOrOne      [\( \?  :_  :Body                   :_* \)]
    :AnyNot         [\( \%  :_  :Body                   :_* \)]
    :Char           [\\ '(| :TabChar :SpaceChar :NewlineChar (% \space))]
    :TabChar        (pegs "tab")
    :SpaceChar      (pegs "space")
    :NewlineChar    (pegs "newline")
    :ValidKeywordChar (lpegs '| "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789:/*+!_?-")
})

(defn- either [n g w] (let [original (gp w)] (first (keep  #(do (sp w original) (pegasus % g w)) (rest n))))
;
; So. The two below are supposed to be 'faster'... when the body you're running is slow-ish... in comparison... and it's not...
;       ... that might be because for most situations the 'backtracking' is only a single character worth
;       ... and the two below could be harder to maintain anyway...
;
    #_(let [[result resultw] (first (remove   #(nil? (first %))
                                            (pmap   #(let [cw (clone w)] [(pegasus % g cw) cw])
                                                    (rest n))))]
        (if (nil? result)
            nil
            (do
                (sp w (gp resultw))
                result)))
    #_(let [[result resultw] (first (drop-while   #(nil? @(first %))
                                                (doall (map     #(let [cw (clone w)] [(future (pegasus % g cw)) cw])
                                                                (rest n)))))]
        (if (nil? result)
            nil
            (do
                (sp w (gp resultw))
                @result)))
)

(defn- type-list [n g w]
    (let [t (first n)
          b (second n)]
        (cond   (= t '|) (let [temp (either n g w)] (debug w "Either returning:" (pr-str temp)) temp)
                (= t '*) (list (doall (take-while   #(if (keyword? b) (b %) %)
                                                    (repeatedly #(pegasus b g w)))))
                (= t '?) (pegasus b g w)
                (= t '%) (let [c (c w)
                               p (gp w)]
                            (if (pegasus b g w)
                                (do                                     ; If we succeed, then we fail - that's the point of AnyNot...
                                    (sp w p) ; Don't forget 'rollback'
                                    nil)
                                (do                                     ; If we fail, then we accept the current char
                                    (debug w "AnyNot MATCH:" (pr-str b))
                                    (m w)
                                    c))))))

(defn- try-char [n w]
    (if (= n (c w))
        (do
            (debug w (str "MATCH: '" (pr-str n) "' with '" (pr-str (c w)) "'"))
            (m w))
        (do
            (debug w (str "FAIL: '" (pr-str n) "' with '" (pr-str (c w)) "'"))
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

(defn- p [w s n] (debug w s (pr-str n)))

(defn pegasus [n g w]
    (in w)
    (let [result (cond
                    (keyword? n)(do (p w "k:" n)    (let [temp (pegasus (n g) g w)] (if temp {n temp} nil)))
                    (vector? n) (do #_(p w "v:" n)  (peg-vec n g w))
                    (list? n)   (do #_(p w "l:" n)  (type-list n g w))
                    (char? n)   (do #_(p w "c:" n)  (try-char n w))
                    true        (throw (Error. (str "Unknown type: " n))))]
    (de w)
    result))

(defn validate
    ([g] (validate g false))
    ([g d]
        (dosync (ref-set *debug* d))
        (let [w (gen-ps (pr-str g))]
            (if (or (nil? (pegasus :Grammar grammar-grammar w))
                    (not (end w)))
                (println "Fail")
                (println "Pass")))
        (dosync (ref-set *debug* false))))

(defn self-check [] (validate grammar-grammar))


; TODO
; 
;   If pegasus is given a keyword, but it doesn't exist in the given grammar, a useful error should be thrown
;   Allow debug to accept a 'limit' - if 0 then always dump everything. If more than 0, only keep limit number of lines of debug and print out at end
;

