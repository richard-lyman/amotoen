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

(defprotocol IPosition (clone [t] "") (m [t] "Returns the 'c' then (inc pos)") (c [t] "The character at pos"))
(defn gen-ps
    ([s] (gen-ps s 0))
    ([s j]
        (let [j (ref j)]
            (reify IPosition
                (clone [t] (gen-ps s @j))
                (m [t] (let [r (c t)] (dosync (alter j inc)) r))
                (c [t] (.charAt s @j))))))

(defn either [n g w]
    (first
        (filter
            #(do
                (println "Filter either" (pr-str %))
                true)
            (doall
                (map
                    #(try (pegasus % g w) (catch Error e nil))
                    (rest n))))))

(defn type-list [n g w]
    (let [t (first n)
          result (cond
                    (= t '|) (either n g w)
                    (= t '*) (doall
                                (take-while
                                    #(if (map? %)
                                        ((second n) %)
                                        (do
                                            (println "Filter *:" %)
                                            false))
                                    (repeatedly #(try (pegasus (second n) g w) (catch Error e nil)))))
                    (= t '?) (try (pegasus (second n) g w) (catch Error e nil))
                    (= t '%) (println "AnyNot"))]
        result))

(defn try-char [n w]
    (if (= n (c w))
        (m w)
        (throw (Error. "Char mismatch"))))

(defn pegasus [n g w]
    (cond
        (keyword? n)(do (println "k:" n) (flush) {n (pegasus (n g) g w)})
        (vector? n) (do (println "v:" n) (flush) (map #(pegasus % g w) n));vec
        (list? n)   (do (println "l:" n) (flush) (type-list n g w))
        (char? n)   (do #_(println "c:" n) (flush) (try-char n w))
        true        (throw (Error. (str "Unknown type: " n)))))

(println "\nDone\n")
(println (pegasus :Grammar grammar-grammar (gen-ps (pr-str {:S \a}))))
