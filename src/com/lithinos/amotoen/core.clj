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
    (:import    (java.util.regex Pattern)
                (java.util UUID)))

(def #^{:private true} grammar-grammar {
    ;:Start              :Expr
    :Start              ['(* " ") "{" ":" "S" " " "\"" "a" "\"" "}"]
    :Expr               [:_* "{" :_* '(+ :Rule) :_* "}" :_*]
; Whitespace
    :Whitespace         '(| " " "\n" "\r" "\t")
    :_*                 '(* :Whitespace)
    :_                  '(+ :Whitespace)
; Non-Terminals
    :Rule               [:_* :Keyword :_ :Body]
    :Keyword            [":" '(+ :ValidKeywordChar)]
    :ValidKeywordChar   '(| "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
                            "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
                            "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" ":" "/" "*" "+" "!" "_" "?" "-")
    :ShortBody          '(| :Keyword :Terminal)
    :Body               '(| :Keyword :Grouping :Terminal)
    :Bodies             [:Body '(* [:_* :Body])]
    :Grouping           '(| :Sequence :Either :ZeroOrMore :OneOrMore :ZeroOrOne :MustFind :MustNotFind)
    :Sequence           ["["        :_*     :Bodies     :_* "]"]
    :Either             [["(" "|"]  :_      :Bodies     :_* ")"]
    :ZeroOrMore         [["(" "*"]  :_      :Body       :_* ")"]
    :OneOrMore          [["(" "+"]  :_      :Body       :_* ")"]
    :ZeroOrOne          [["(" "?"]  :_      :Body       :_* ")"] ; Not used in grammar-grammar
    :MustFind           [["(" "&"]  :_      :Body       :_* ")"] ; Not used in grammar-grammar
    :MustNotFind        [["(" "!"]  :_      :Body       :_* ")"] ; Not used in grammar-grammar
    :Until              [["(" "%"]  :_      :ShortBody  :_* ")"]
    :Terminal           '(| :DoubleQuotedString)
; Terminals
    :DoubleQuotedString ["\"" '(+ :DoubleQuotedStringContent) "\""] ; This allows for multi-char terminals...
        :DoubleQuotedStringContent  '(| :EscapedSlash :EscapedDoubleQuote :AnyNotDoubleQuote)
            :EscapedSlash               ["\\" "\\"]
            :EscapedDoubleQuote         ["\\" "\""]
            :AnyNotDoubleQuote          '(% "\"")
})

(def *indent* (ref 0))
(defn gen-indent []
    (loop [result   ""
           count    @*indent*]
        (if (< 0 count)
            (recur (str result "  ") (dec count))
            result)))
(defn mark [z]
    (println
        "NODE:"
        (try (pr-str (z/node z)) (catch Exception e "NO NODE"))
        "\n\tLEFT"
        (try (pr-str (z/lefts z)) (catch Exception e "NO LEFT"))
        "\n\tRIGHT"
        (try (pr-str (z/rights z)) (catch Exception e "NO RIGHT"))
        "\n\tPREV" 
        (try (pr-str (z/node (z/prev z))) (catch Exception e "NO PREV"))
        "\n\tCHILDREN"
        (try (pr-str (z/children z)) (catch Exception e "NO CHILDREN"))
        "\n\tPATH"
        (try (pr-str (z/path z)) (catch Exception e "NO PATH"))
        "\n\tROOT"
        (try (pr-str (z/root z)) (catch Exception e "NO ROOT")))
    z)

(def *fail-node* :this-marks-some-failed-evolution)
(defn fail [z] (println "Failing:" (z/node z)) (mark z) (z/up (z/replace z *fail-node*)))
(defn failed? [z] (= (z/node z) *fail-node*))
(defn cleanup [z]
    (println "Removing:" (z/node z) (mark z))
    (if (or (= (z/leftmost z) z)
            (nil? (z/prev z)))
        (do (println "Upping before remove:" (z/node z) (mark z))
            z);(-> z z/up z/remove))
        (z/up z)));(z/remove z)))

(defprotocol wrapped-input
    (has? [t] "")
    (move [t] "")
    (back [t] "")
    (roll [t n] "")
    (loca [t] "")
    (curr [t] ""))

(defn wrap [input]
    (let [location (ref 0)]
        (reify wrapped-input
            (has? [t] (< (inc @location) (count input)))
            (move [t] (dosync (alter location inc)))
            (back [t] (dosync (alter location dec)))
            (roll [t n] (dosync (ref-set location n)))
            (loca [t] @location)
            (curr [t] (if (<= (count input) @location) "" (subs input @location (inc @location)))))))

(declare evolve)
(defn expose [z] (pr-str (z/root z)))
(defn end [z m]
    (do (println m)
        (println "Last known good:")
        (println (pprint (expose z)))
        (System/exit -1)))

(def *cycle-map* (ref {}))
(defn keyword-evolution [r z g i]
    (println (gen-indent) r (pr-str (curr i)))
    (when (r @*cycle-map*) (end z "Infinite cycle"))
    (dosync (alter *cycle-map* assoc r (loca i)))
    (dosync (alter *indent* inc))
    (let [result
                    (if (z/prev z)
                        (evolve (r g) (-> z (z/insert-right [r]) z/right z/down) g i)
                        (evolve (r g) (-> z (z/insert-child r) z/down) g i))]
        (dosync (alter *cycle-map* dissoc r))
        (dosync (alter *indent* dec))
        result))

(defn vector-evolution [r z g i]
    (loop [remaining    (rest r)
           z            (evolve (first r) z g i)]
        (if (failed? z)
            (fail (z/up z))
            (if (seq remaining)
                (recur  (rest remaining)
                        (evolve (first remaining) (z/rightmost z) g i))
                (z/up z)))))

(defn zero-or-more-evolution [r z g i]
    (println "Trying:" (pr-str r) (pr-str (curr i)))
    (loop [result (evolve r z g i)]
        (if (failed? result)
            (do (println "Failed:" (pr-str r) (pr-str (curr i))) (cleanup result))
            (do (println "Success:" (pr-str r) (pr-str (curr i)) (mark result)) (recur (evolve r result g i))))))

(defn either-evolution [list-body z g i]
    (let [rollback (loca i)]
        (loop [remaining list-body]
            (let [attempt (evolve (first remaining) z g i)]
                (if (failed? attempt)
                    (if (seq (rest remaining))
                        (do (roll i rollback) (recur (rest remaining)))
                        (fail z))
                    (z/up attempt))))))

(defn one-or-more-evolution [body z g i]
    (let [first-result (evolve body z g i)]
        (if (failed? first-result)
            (fail z)
            (z/up (zero-or-more-evolution body first-result g i)))))

(defn until-evolution [body z g i]
    (if (failed? (evolve body z g i))
        (let [result (-> z (z/insert-right (curr i)) z/right)]
            (dosync (ref-set *cycle-map* {}))
            (move i)
            result)
        (do (println (gen-indent) "Failing on until of" (pr-str body)) (back i) (fail z))))

(defn list-evolution [r z g i]
    (let [list-type (first r)
          list-body (rest r)
          z         (-> z (z/insert-right [list-type]) z/right z/down)]
        (cond
            (= list-type '*) (zero-or-more-evolution (first list-body) z g i)
            (= list-type '|) (either-evolution list-body z g i)
            (= list-type '+) (one-or-more-evolution (first list-body) z g i)
            (= list-type '%) (until-evolution (first list-body) z g i)   ; This  will eventually be more...
            true (end z (str "Unknown list-type: " (pr-str list-type))))))

(defn string-evolution [r z g i]
    (println "String evo:" (pr-str r) (pr-str (curr i)))
    (if (< 1 (count r))
        (end z "Unable to handle multi-char terminals")
        (if (not= r (curr i))
            (do (println "Str-evo failed") (fail z))
            (do (println "Str-evo success") (let [result (-> z (z/insert-right (curr i)) z/right)]
                (dosync (ref-set *cycle-map* {}))
                (move i)
                (println (gen-indent) "Match:" r)
                result)))))

(defn evolve [r z g i]
    (println "\t" (pr-str r))
    (cond
        (keyword? r)    (keyword-evolution r z g i)
        (vector? r)     (vector-evolution r z g i)
        (list? r)       (list-evolution r z g i)
        (string? r)     (string-evolution r z g i)
        true (end z (str "Unknown rule type:" (pr-str r)))))

;(defn pegasus [grammar input-wrapped]
;    (loop [asts (list (z/vector-zip []))]
;        (if (has? input-wrapped)
;            (recur  (doall (map #(evolve :Start % grammar input-wrapped) asts)) ; Flatten and de-nullify
;                    )
;            (expose (first (doall (map #(evolve :Something-goes-here % grammar input-wrapped) asts)))))))
(defn pegasus [grammar input-wrapped]
    (evolve (:Start grammar) (z/down (z/vector-zip [:Start])) grammar input-wrapped))

(let [result (pegasus grammar-grammar (wrap "{:S \"a\"}"))]
    (println (pprint (z/root result))))


; user=> (require '[clojure.zip :as z])
; user=> (def z (ref (-> (vector-zip [:Start]) down)))
; user=> (node @z)
; :Start
; user=> (dosync (ref-set z (-> @z (insert-right [:Bob]) right down)))
; user=> (root @z)
; [:Start [:Bob]]
; user=> (dosync (ref-set z (-> @z (insert-right :qwe) right)))
; user=> (root @z)
; [:Start [:Bob :qwe]]

