;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.core)

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
    :Char           [\\ '(| :Tab :Space :Newline (% \space)) \space]
    :Tab            [\t \a \b]
    :Space          [\s \p \a \c \e]
    :Newline        [\n \e \w \l \i \n \e]
    :ValidKeywordChar '(| \A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z
                        \a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z
                        \0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \: \/ \* \+ \! \_ \? \-)
})

(declare pegasus)
(def j -1)

(def *indent* (ref -1))
(defn in [] (apply str (take @*indent* (repeat " "))))

(defn type-list [n g i]
    (let [t (first n)
          result (cond
                            ; Close but not. There needs to be something that 'checks' to see if the future should continue...
                            ; .. and a new binding... that returns j...
                    (= t '|) @(first (filter #(not (nil? %)) (doall (map #(binding [j j] (println "Future:" (pr-str %)) (future (pegasus % g i))) (rest n)))))
                    (= t '*) (doall (take-while #(not (nil? %)) (repeatedly (fn [] (try (pegasus (second n) g i) (catch Error e nil))))))
                    (= t '?) (try (pegasus (second n) g i) (catch Error e nil))
                    (= t '%) (println "AnyNot"))]
        (println "List returning:" result)
        result))

(defn pegasus [n g i]
    (println (in) (pr-str n))
    (dosync (alter *indent* inc))
    (let [result    (cond
                        (keyword? n){n (pegasus (n g) g i)}
                        (vector? n) (vec (map #(pegasus % g i) n))
                        (list? n)   (type-list n g i)
                        (char? n)   (if (= n (first (subs i j (inc j))))
                                        (do (println "MATCH") (set! j (inc j)) n)
                                        (throw (Error. "Char mismatch")))
                        true        (throw (Error. (str "Unknown type: " n))))]
        (dosync (alter *indent* dec))
        result))

(println "\nDone\n" (binding [j 0] (pegasus :Grammar grammar-grammar (pr-str {:S \a}))))
