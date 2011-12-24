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
    :Char           [\\ '(| 
                            ;:TabChar :SpaceChar :NewlineChar 
                            (% \space)) \space]
    ;:TabChar        [\t \a \b]
    ;:SpaceChar      [\s \p \a \c \e]
    ;:NewlineChar    [\n \e \w \l \i \n \e]
    :ValidKeywordChar '(| \A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z
                        \a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z
                        \0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \: \/ \* \+ \! \_ \? \-)
})

(declare pegasus)
(def j -1)

(defn in [] "")

(defn type-list [n g i]
    ;(println "Processing at:" j)
    (let [tempj j
          t (first n)
          result (cond
                    (= t '|) (first
                                (filter
                                    #(not (nil? %))
                                    (doall
                                        (pmap ; Is there a way to stop the pmap if one of the options is valid?
                                            ;
                                            ;   THIS NEEDS TO 'MOVE' J IF IT WAS SUCCESSFUL...
                                            ;
                                            #(binding [j tempj]
                                                ;(println "With:" j)
                                                (try (pegasus % g i) (catch Error e nil)))
                                            (rest n)))))
                    (= t '*) (doall
                                (take-while
                                    #(do
                                        ;(println "Testing zero-or-more:" ((second n) %)) 
                                        (not (nil? ((second n) %))))
                                    (repeatedly (fn [] (try (pegasus (second n) g i) (catch Error e nil))))))
                    (= t '?) (try (pegasus (second n) g i) (catch Error e nil))
                    (= t '%) (println "AnyNot"))]
        ;(println "List" t "returning:" result)
        result))

(defn p [s n] (println (in) s (pr-str n)))

(defn try-char [n i]
    ;(println "Using:" j)
    ;(println (str "Trying to match char: '" (pr-str n) "' to: '" (pr-str (first (subs i j (inc j)))) "' out of: '" (pr-str (subs i j)) "'"))
    (if (= n (first (subs i j (inc j))))
        (do (p "MATCH" n)  (set! j (inc j)) n)
        (throw (Error. "Char mismatch"))))

(defn pegasus [n g i]
    (cond
        (keyword? n) (do (p "k" n) {n (pegasus (n g) g i)})
        (vector? n) (do (p "v" n) (vec (map #(pegasus % g i) n)))
        (list? n)   (do (p "l" n) (type-list n g i))
        (char? n)   (do
                        ;(p "c" n)
                        (try-char n i))
        true        (do (p "e" n) (throw (Error. (str "Unknown type: " n))))))

(println "\nDone\n" (binding [j 0] (pegasus :Grammar grammar-grammar (pr-str {:S \a}))))
