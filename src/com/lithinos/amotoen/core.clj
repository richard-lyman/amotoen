;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.core)

(def #^{:private true} grammar-grammar {
    :Start          :Grammar
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
    :Char           [\\ '(| :Tab :Space :Newline (% \space)) \space]
    :Tab            [\t \a \b]
    :Space          [\s \p \a \c \e]
    :Newline        [\n \e \w \l \i \n \e]
    :ValidKeywordChar '(| \A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z
                        \a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z
                        \0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \: \/ \* \+ \! \_ \? \-)
})

(def *indent* (ref -1))
(defn inden [] (apply str (take @*indent* (repeat " "))))

(defn pegasus
    ([g i] (pegasus :Start g i 0))
    ([n g i j]
        ;(println (inden) "Processing" n)
        (dosync (alter *indent* inc))
        (let [result    (cond
                            (keyword? n)(do (println (inden) n) {n (pegasus (n g) g i j)})
                            (vector? n) (do (println (inden) n) (map #(pegasus % g i j) n))
                            (list? n)   (do (println (inden) n) "list")
                            (char? n)   (do (println (inden) n) "char")
                            true        (do (println (inden) n "UNKNOWN") "?"))]
            (dosync (alter *indent* dec))
            result)))

(println "\nDone\n" (pegasus grammar-grammar (pr-str grammar-grammar)))
