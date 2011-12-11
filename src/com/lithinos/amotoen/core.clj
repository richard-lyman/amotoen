;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.core
    (:use (com.lithinos.amotoen errors string-wrapper wrapper))
    (:require [clojure.zip :as z])
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

(defprotocol darwin-ast
    (expose [t] "")
    (evolve [t c] ""))

(defn explode [grammar]
    (let [z (ref (-> (z/vector-zip [:Start]) z/down))]
        (reify darwin-ast
            (expose [t] (z/root @z))
            (evolve [t c]
                (println "Evolving from:" (z/node @z) "using:" c)
                t))))

(defn pegasus [grammar input-wrapped]
    (loop [asts (list (explode grammar))
           c    (curr input-wrapped)]
        (if (has? input-wrapped)
            (recur  (doall (map #(evolve % c) asts)) (do (move input-wrapped) (curr input-wrapped)))
                    (doall (map #(evolve % c) asts)))))

(pegasus grammar-grammar (wrap "{:S \"a\"}"))



; user=> (def z (ref (-> (vector-zip [:Start]) down)))
; user=> (node @z)
; :Start
; user=> (dosync (ref-set z (-> @z (insert-right [:Bob]) right down)))
; user=> (root @z)
; [:Start [:Bob]]
; user=> (dosync (ref-set z (-> @z (insert-right :qwe) right)))
; user=> (root @z)
; [:Start [:Bob :qwe]]

