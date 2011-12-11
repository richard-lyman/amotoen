;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.core
    (:use (com.lithinos.amotoen errors string-wrapper wrapper)
            (clojure pprint))
    (:require   [clojure.zip :as z])
    (:import (java.util.regex Pattern)))

(def #^{:private true} grammar-grammar {
    :Start              :Expr
    :Expr               [:_* "{" :_* '(+ :Rule) :_* "}" :_* :$]
; Whitespace
    :Whitespace         '(| " " "\n" "\r" "\t")
    :_*                 '(* :Whitespace)
    :_                  '(+ :Whitespace)
; Non-Terminals
    :Rule               [:_* :Keyword :_ :Body :_* "," :_*]
    :Keyword            [":" '(+ :ValidKeywordChar)]
    :ValidKeywordChar   '(| "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
                            "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
                            "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" ":" "/" "*" "+" "!" "_" "?" "-")
    :ShortBody          '(| :Keyword :Terminal)
    :Body               '(| :Keyword :Grouping :Terminal)
    :Bodies             [:Body '(* [:_* :Body])]
    :Grouping           '(| :Sequence :Either :ZeroOrMore :OneOrMore :ZeroOrOne :MustFind :MustNotFind)
    :Sequence           ["["    :_*                 :Bodies     :_* "]"]
    :Either             ["(|"   :_                  :Bodies     :_* ")"]
    :ZeroOrMore         ["(*"   :_                  :Body       :_* ")"]
    :OneOrMore          ["(+"   :_                  :Body       :_* ")"]
    :ZeroOrOne          ["(?"   :_                  :Body       :_* ")"] ; Not used in grammar-grammar
    :MustFind           ["(&"   :_                  :Body       :_* ")"] ; Not used in grammar-grammar
    :MustNotFind        ["(!"   :_                  :Body       :_* ")"] ; Not used in grammar-grammar
    :Until              ["(="   :_                  :ShortBody  :_* ")"]
    :Terminal           '(| :DoubleQuotedString :EndOfInput)
; Terminals
    :EndOfInput         ":$"
    :DoubleQuotedString ["\"" '(+ :DoubleQuotedStringContent) "\""]
        :DoubleQuotedStringContent  '(| :EscapedSlash :EscapedDoubleQuote :AnyNotDoubleQuote)
            :EscapedSlash               ["\\" "\\"]
            :EscapedDoubleQuote         ["\\" "\""]
            :AnyNotDoubleQuote          '(= "\"")
})

(defprotocol wrapped-input
    (has? [t] "")
    (move [t] "")
    (curr [t] ""))

(defn wrap [input]
    (let [location (ref 0)]
        (reify wrapped-input
            (has? [t] (< (inc @location) (count input)))
            (move [t] (dosync (alter location inc)))
            (curr [t] (subs input @location (inc @location))))))

(declare evolve)
(defn expose [z] (z/root z))
(defn end [z m]
    (do (println m)
        (println "Last known good:")
        (println (pprint (expose z)))
        (System/exit -1)))

(defn keyword-evolution [r z g c]
    (if (= [] (z/node z))
        (evolve (r g) (-> z (z/insert-child r) z/down) g c)
        (evolve (r g) (-> z (z/insert-right [r]) z/right z/down) g c)))

(defn vector-evolution [r z g c]
(let [z (-> z (z/insert-right []) z/right (z/insert-child []) z/down)]
    (loop [remaining    (rest r)
           z            (-> z (z/insert-child [(evolve (first r) z g c)]) z/down)]
        (if (seq remaining)
            (recur  (-> z (z/insert-right (evolve (first remaining) z g c)) z/right)
                    (rest remaining))
            z))))

(defn zero-or-more-evolution [body z g c]
    (loop [result z]
        (let [attempt-to-get-another (evolve body z g c)]
            (if attempt-to-get-another ; will be nil when it fails
                (recur attempt-to-get-another)
                result))))

(defn either-evolution [list-body z g c]
    (loop [remaining list-body]
        (let [attempt (evolve (first remaining) z g c)]
            (if attempt ; will be nil when it fails
                attempt
                (if (seq (rest remaining))
                    (recur (rest remaining))
                    (end (-> z z/up z/remove) "End of either... still no match"))))))

(defn list-evolution [r z g c]
    (let [list-type (first r)
          list-body (rest r)]
        (cond
            (= list-type '*) (zero-or-more-evolution (first list-body) (-> z (z/insert-right []) z/right) g c)
            (= list-type '|) (either-evolution list-body z g c)
            true (end z (str "Unknown list-type: " list-type)))))

(defn string-evolution [r z g c]
    (if (< 1 (count r))
        (end z "Unable to handle multi-char terminals")
        (if (= r c)
            (-> z (z/insert-right c) z/right)
            nil ; Somehow fail...
            )))

(defn evolve [r z g c]
    (println (expose z) "\t\t" (pr-str r))
    (cond
        (keyword? r)    (keyword-evolution r z g c)
        (vector? r)     (vector-evolution r z g c)
        (list? r)       (list-evolution r z g c)
        (string? r)     (string-evolution r z g c)
        true (end z (str "Unknown rule type:" (pr-str r)))))

(defn pegasus [grammar input-wrapped]
    (loop [asts (list (-> (z/vector-zip [])))
           c    (curr input-wrapped)]
        (if (has? input-wrapped)
            (recur  (doall (map #(evolve :Start % grammar c) asts)) ; Flatten and de-nullify
                    (do (move input-wrapped) (curr input-wrapped)))
            (expose (first (doall (map #(evolve :Something-goes-here % grammar c) asts)))))))

(let [result (pegasus grammar-grammar (wrap "{:S \"a\"}"))]
    (println (pprint result)))



; user=> (def z (ref (-> (vector-zip [:Start]) down)))
; user=> (node @z)
; :Start
; user=> (dosync (ref-set z (-> @z (insert-right [:Bob]) right down)))
; user=> (root @z)
; [:Start [:Bob]]
; user=> (dosync (ref-set z (-> @z (insert-right :qwe) right)))
; user=> (root @z)
; [:Start [:Bob :qwe]]

