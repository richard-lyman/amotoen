;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.core
    (import (java.util UUID)))

(def #^{:private true} grammar-grammar {
    :Whitespace     '(| \space \newline \tab)
    :_*             '(* :Whitespace)
    :_              [:Whitespace '(* :Whitespace)]
    :Grammar        [\{ :_* :Rule '(* [:_ :Rule]) :_* \}]
    :Rule           [:Keyword :_ :Body]
    :Keyword        [\: :ValidKeywordChar '(* :ValidKeywordChar)]
    :Body           '(| :Keyword :Grouping :DoubleQuotedString)
    :Grouping       '(| :Sequence :Either :ZeroOrMore :ZeroOrOne :MustNotFind :AnyNot)
    :Sequence       [\[         :_* [:Body '(* [:_* :Body])]    :_* \]]
    :Either         [[\( \|]    :_  [:Body '(* [:_* :Body])]    :_* \)]
    :ZeroOrMore     [[\( \*]    :_  :Body                       :_* \)]
    :ZeroOrOne      [[\( \?]    :_  :Body                       :_* \)]
    :AnyNot         [[\( \%]    :_  '(| :Keyword :Char)         :_* \)]
    :Char           [\\ '(| :TabChar :SpaceChar :NewlineChar (% \space)) \space]
    :TabChar        [\t \a \b]
    :SpaceChar      [\s \p \a \c \e]
    :NewlineChar    [\n \e \w \l \i \n \e]
    :ValidKeywordChar '(| \A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z
                        \a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z
                        \0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \: \/ \* \+ \! \_ \? \-)
})

(declare pegasus)
(def j -1)

(defprotocol IPosition (clone [t] "") (gpos [t] "") (spos [t j] ""))
(defn gen-pos-str
    ([s] (gen-pos-str s 0))
    ([s j]
        (let [j (ref j)]
            (reify IPosition
                (clone [t] (gen-pos-str s (gpos t)))
                (gpos [t] @j)
                (spos [t k] (dosync (ref-set j k)))))))

;(def ps (gen-pos-str "asd"))
;(println "Get:" (gpos ps))
;(println "Set:" (spos ps 10))
;(println "Get:" (gpos ps))

(defn in [] "")

(defn either [n g i]
    (first
        (filter
            #(not (nil? (second %)))
            (doall
                (map
                    #(try (pegasus % g i) (catch Error e nil))
                    (rest n))))))

(defn type-list [n g i]
    (let [t (first n)
          result (cond
                    (= t '|) (either n g i)
                    (= t '*) (doall
                                (take-while
                                    #(not (nil? ((second n) %)))
                                    (repeatedly (try (pegasus (second n) g i) (catch Error e nil)))))
                    (= t '?) (try (pegasus (second n) g i) (catch Error e nil))
                    (= t '%) (println "AnyNot"))]
        result))

(defn try-char [n i]
    (if (= n (first (subs i j (inc j))))
        (do (set! j (inc j)) n)
        (throw (Error. "Char mismatch"))))

(defn pegasus [n g i]
    (println n)
    (cond
        (keyword? n) {n (pegasus (n g) g i)}
        (vector? n) (vec (map #(pegasus % g i) n))
        (list? n)   (type-list n g i)
        (char? n)   (try-char n i)
        true        (throw (Error. (str "Unknown type: " n)))))

;(println "\nDone\n" (binding [j 0] (pegasus :Grammar grammar-grammar (pr-str {:S \a}))))
