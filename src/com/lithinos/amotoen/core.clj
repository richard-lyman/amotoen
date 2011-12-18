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

(defprotocol flyable
    (step [t c] "")
    (dest [t] ""))

(defn wings
    ([grammar]
        (wings grammar (z/down (z/vector-zip [:Start]))))
    ([grammar z]
        (reify flyable
            (step [t c]
                (println "Trying:" c)
                (let [newz  (let [n (z/node z)]
                                (cond
                                    (keyword? n) (do (println "Processing keyword:" n) [z])
                                    true (do (println "Unknown node type:" n) [z])))]
                    (map #(wings grammar %) newz)))
                ;[(wings grammar z)]) ; Modify the zipper 'z' passed in below to reflect the new AST. If there's more than one zipper, there needs to be more than one wings call
            (dest [t] (z/root z)))))

(defn pegasus [grammar i]
    (loop [l    0
           asts [(wings grammar)]]
        (if (= l (count i))
            (first asts)
            (recur  (inc l)
                    (remove nil? (flatten (map #(step % (subs i l (inc l)))  asts)))))))

(let [result (pegasus {:Start ["a"]} "a" )]
    (println (pprint (dest result))))


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


;(def *indent* (ref 0))
;(defn gen-indent [] (apply str (take @*indent* (repeat "  "))))
;(defn mark [z]
;    (println
;        "NODE:"         (try (pr-str (z/node z)) (catch Exception e "NO NODE"))
;        "\n\tLEFT"      (try (pr-str (z/lefts z)) (catch Exception e "NO LEFT"))
;        "\n\tRIGHT"     (try (pr-str (z/rights z)) (catch Exception e "NO RIGHT"))
;        "\n\tPREV"      (try (pr-str (z/node (z/prev z))) (catch Exception e "NO PREV"))
;        "\n\tCHILDREN"  (try (pr-str (z/children z)) (catch Exception e "NO CHILDREN"))
;        "\n\tPATH"      (try (pr-str (z/path z)) (catch Exception e "NO PATH"))
;        "\n\tROOT"      (try (pr-str (z/root z)) (catch Exception e "NO ROOT")))
;    z)

;(defn vector-evolution [r z g i]
;    z)

;(defn string-evolution [r z g i]
;    z)

;(defn evolve [r z g i]
;    (cond
;;        (keyword? r)    (keyword-evolution r z g i)
;        (vector? r)     (vector-evolution r z g i)
;;        (list? r)       (list-evolution r z g i)
;        (string? r)     (string-evolution r z g i)
;        true (end z (str "Unknown rule type:" (pr-str r)))))

