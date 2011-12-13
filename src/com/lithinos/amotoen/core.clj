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

(def *fail-node* :this-marks-some-failed-evolution)
(defn fail [z] (-> z (z/replace *fail-node*)))
(defn failed? [z] (= (z/node z) *fail-node*))
(defn cleanup [z] (-> z z/remove))
(defn mark [z] (println (z/lefts z) "X" (z/rights z) "IN" (z/node (z/prev z)) "HAVING" (z/children z) "THROUGH" (z/path z) "FULL" (z/root z)) z)

(defprotocol wrapped-input
    (has? [t] "")
    (move [t] "")
    (loca [t] "")
    (curr [t] ""))

(defn wrap [input]
    (let [location (ref 0)]
        (reify wrapped-input
            (has? [t] (< (inc @location) (count input)))
            (move [t] (dosync (alter location inc)))
            (loca [t] @location)
            (curr [t] (subs input @location (inc @location))))))

(declare evolve)
(defn expose [z] (pr-str (z/root z)))
(defn end [z m]
    (do (println m)
        (println "Last known good:")
        (println (pprint (expose z)))
        (System/exit -1)))

(def *cycle-map* (ref {}))
(defn keyword-evolution [r z g i]
    (println (pr-str (curr i)) r (r @*cycle-map*))
    (when (r @*cycle-map*) (end z "Infinite cycle"))
    (dosync (alter *cycle-map* assoc r (loca i))) ; When a rule leave's itself, it should be removed... or something
    (let [result    (if (= [] (z/node z))
                        (evolve (r g) (-> z (z/insert-child r) z/down) g i)
                        (evolve (r g) (-> z (z/insert-right [r]) z/right z/down) g i))]
        (dosync (alter *cycle-map* dissoc r))
        result))

(defn vector-evolution [r z g i]
    (let [z (-> z (z/insert-right []) z/right (z/insert-child []) z/down)] ; This isn't always what's wanted...
        (loop [remaining    (rest r)
               z            (evolve (first r) z g i)] ; If this fails everything should fail
            (if (seq remaining)
                (recur  (rest remaining)
                        (evolve (first remaining) (-> z z/rightmost) g i))
                z))))

(defn zero-or-more-evolution [body z g i]
    (loop [result z]
        (if (failed? result)
            (z/up (cleanup result))
            (recur (evolve body result g i)))))

(defn either-evolution [list-body z g i]
    (loop [remaining list-body]
        (let [attempt (evolve (first remaining) z g i)]
            (if (failed? attempt)
                (if (seq (rest remaining))
                    (recur (rest remaining))
                    (fail z))
                attempt))))

(defn one-or-more-evolution [body z g i]
    (let [first-result (evolve body z g i)]
        (if (failed? first-result)
            (fail z)
            (zero-or-more-evolution body first-result g i))))

(defn list-evolution [r z g i]
    (let [list-type (first r)
          list-body (rest r)]
        (cond
            (= list-type '*) (zero-or-more-evolution (first list-body) (-> z (z/insert-right []) z/right) g i)
            (= list-type '|) (either-evolution list-body z g i)
            (= list-type '+) (one-or-more-evolution (first list-body) (-> z (z/insert-right []) z/right) g i)
            true (end z (str "Unknown list-type: " list-type)))))

; We need to move to the next char
(defn string-evolution [r z g i]
    (if (< 1 (count r))
        (end z "Unable to handle multi-char terminals")
        (if (= r (curr i))
            (let [result (-> z (z/insert-right (curr i)) z/right)]
                (println "MATCH!" (pr-str r) (expose result))
                (dosync (ref-set *cycle-map* {}))
                (move i)
                result)
            (fail z))))

(defn evolve [r z g i]
    (println "\t" (pr-str r))
    (cond
        (keyword? r)    (keyword-evolution r z g i)
        (vector? r)     (vector-evolution r z g i)
        (list? r)       (list-evolution r z g i)
        (string? r)     (string-evolution r z g i)
        true (end z (str "Unknown rule type:" (pr-str r)))))

(defn pegasus [grammar input-wrapped]
    (loop [asts (list (-> (z/vector-zip [])))]
        (if (has? input-wrapped)
            (recur  (doall (map #(evolve :Start % grammar input-wrapped) asts)) ; Flatten and de-nullify
                    )
            (expose (first (doall (map #(evolve :Something-goes-here % grammar input-wrapped) asts)))))))

(let [result (pegasus grammar-grammar (wrap "{:S \"a\"}"))]
    (println (pprint (z/root result))))



; user=> (def z (ref (-> (vector-zip [:Start]) down)))
; user=> (node @z)
; :Start
; user=> (dosync (ref-set z (-> @z (insert-right [:Bob]) right down)))
; user=> (root @z)
; [:Start [:Bob]]
; user=> (dosync (ref-set z (-> @z (insert-right :qwe) right)))
; user=> (root @z)
; [:Start [:Bob :qwe]]

