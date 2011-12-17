;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.core
    (:use (clojure pprint))
    (:require   [clojure.zip :as z])
    (:import    (java.util.regex Pattern)))

(def #^{:private true} grammar-grammar {
    :Start              :Grammar
    :Whitespace         '(| " " "\n" "\r" "\t")
    :_*                 '(* :Whitespace)
    :_                  [:Whitespace '(* :Whitespace)]
; Non-Terminals
    :Grammar            ["{" :Rule '(* :Rule) :_* "}"]
    :Rule               [:_* :Keyword :_ :Body]
    :Keyword            [":" :ValidKeywordChar '(* :ValidKeywordChar)]
    :ValidKeywordChar   '(| "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
                            "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
                            "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" ":" "/" "*" "+" "!" "_" "?" "-")
    :Body               '(| :Keyword :Grouping :DoubleQuotedString)
    :Grouping           '(| :Sequence :Either :ZeroOrMore :ZeroOrOne :MustNotFind :AnyNot)
    :Sequence           ["["        :_* [:Body '(* [:_* :Body])]    :_* "]"]
    :Either             [["(" "|"]  :_  [:Body '(* [:_* :Body])]    :_* ")"]
    :ZeroOrMore         [["(" "*"]  :_  :Body                       :_* ")"]
    :ZeroOrOne          [["(" "?"]  :_  :Body                       :_* ")"]
    :AnyNot             [["(" "%"]  :_  '(| :Keyword :QuotedChar)   :_* ")"]
; Terminal
    :QuotedChar         ["\"" '(| :EscapedSlash :EscapedDoubleQuote :AnyNotDoubleQuote) "\""]
    :EscapedSlash       ["\\" "\\"]
    :EscapedDoubleQuote ["\\" "\""]
    :AnyNotDoubleQuote  '(% "\"")
})

(def *indent* (ref 0))
(defn gen-indent [] (apply str (take @*indent* (repeat "  "))))
(defn mark [z]
    (println
        "NODE:"         (try (pr-str (z/node z)) (catch Exception e "NO NODE"))
        "\n\tLEFT"      (try (pr-str (z/lefts z)) (catch Exception e "NO LEFT"))
        "\n\tRIGHT"     (try (pr-str (z/rights z)) (catch Exception e "NO RIGHT"))
        "\n\tPREV"      (try (pr-str (z/node (z/prev z))) (catch Exception e "NO PREV"))
        "\n\tCHILDREN"  (try (pr-str (z/children z)) (catch Exception e "NO CHILDREN"))
        "\n\tPATH"      (try (pr-str (z/path z)) (catch Exception e "NO PATH"))
        "\n\tROOT"      (try (pr-str (z/root z)) (catch Exception e "NO ROOT")))
    z)

(defprotocol wrapped-input
    (has? [t] "")
    (move [t] "")
    (curr [t] ""))

(defn wrap [input]
    (let [location (ref 0)]
        (reify wrapped-input
            (has? [t] (< (inc @location) (count input)))
            (move [t] (dosync (alter location inc)))
            (curr [t] (if (<= (count input) @location) "" (subs input @location (inc @location)))))))

(declare evolve)
(defn end [z m] (println m) (println "Last known good:") (mark z) (System/exit -1))

(defn vector-evolution [r z g i]
    (println "Vector:" r)
    ; A Vector evals each sub-part in order
    ; Since Keyword places the Non-Terminal as the first item in the list and the z/downs into the list, we can z/insert-right
    ; We do _not_ z/up at the end... the Keyword must do that, since it z/down'd
    (println "Vector on:" z)
    (loop [remaining r
           z z]
        (if (seq remaining)
            (recur (rest remaining) (-> z (z/insert-right (evolve (first remaining) z g i)) z/right))
            z)))

(defn string-evolution [r z g i]
    z)

; This function is meant to return one of two things... a valid ast or nil if there is no valid ast
(defn evolve [r z g i]
    ;(println "\t" (pr-str r))
    (cond
;        (keyword? r)    (keyword-evolution r z g i)
        (vector? r)     (vector-evolution r z g i)
;        (list? r)       (list-evolution r z g i)
        (string? r)     (string-evolution r z g i)
        true (end z (str "Unknown rule type:" (pr-str r)))))

(defn pegasus [grammar input-wrapped]
    (evolve (:Start grammar) (z/down (z/vector-zip [:Start])) grammar input-wrapped))

;(wrap "{:S \"a\"}")
(let [result (pegasus
                { :Start ["a"] }
                (wrap "a")
                )]
    (println (pprint (z/root result))))


; user=> (require '[clojure.zip :as z]) ; Except not :as z
; user=> (def z (ref (-> (vector-zip [:Start]) down)))
; user=> (node @z)
; :Start
; user=> (dosync (ref-set z (-> @z (insert-right [:Bob]) right down)))
; user=> (root @z)
; [:Start [:Bob]]
; user=> (dosync (ref-set z (-> @z (insert-right :qwe) right)))
; user=> (root @z)
; [:Start [:Bob :qwe]]

