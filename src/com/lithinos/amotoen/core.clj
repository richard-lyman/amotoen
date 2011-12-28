;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.core
    (:use clojure.pprint))

(declare pegasus)
(defprotocol IPosition
    (psdebug [t] "Some form of helpful debug info")
    (clone [t] "")
    (gp [t] "Returns pos") ; E.V.I.L.
    (sp [t j] "Sets pos") ; E.V.I.L.
    (in [t] "Indent")
    (de [t] "Dedent")
    (m [t] "Returns the 'c' then (inc pos)")
    (c [t] "The character at pos"))
(defn gen-ps
    ([s] (gen-ps s 0))
    ([s j]
        (let [j (ref j)
              indent (ref 0)]
            (reify IPosition
                (psdebug [t]
                    (str    (if (< @j 0)
                                (str "<-" (subs s 0 20))
                                (str    "'" (subs s (max 0 (- @j 30)) (max 0 @j)) "'"
                                        " " (c t) " "
                                        "'" (subs s (inc @j) (min (+ @j 30) (count s))) "'"))
                            (apply str (take @indent (repeat "  ")))))
                (in [t] (dosync (alter indent inc)))
                (de [t] (dosync (alter indent dec)))
                (gp [t] @j)
                (sp [t k] (dosync (ref-set j k)))
                (clone [t] (gen-ps s @j))
                (m [t] (let [r (c t)] (dosync (alter j inc)) r))
                (c [t] (.charAt s @j))))))
(defn lpegs [t s] (reverse (into '() (cons t (seq s))))) ; This doesn't need to be fast, but shouldn't the following work? (list (cons t (seq s)))
(defn pegs [s] (vec (seq s)))


(defn- debug [w & args] #_(do (print (psdebug w)) (apply println args) (flush)))

(def #^{:private true} grammar-grammar {
    :Whitespace     '(| \space \newline \tab \,)
    :_*             '(* :Whitespace)
    :_              [:Whitespace '(* :Whitespace)]
    :Grammar        [\{ :_* :Rule '(* [:_ :Rule]) :_* \}]
    :Rule           [:Keyword :_ :Body]
    :Keyword        [\: :ValidKeywordChar '(* :ValidKeywordChar)]
    :Body           '(| :Keyword :Grouping :Char)
    :Grouping       '(| :Sequence :Either :ZeroOrMore :ZeroOrOne :AnyNot)
    :Sequence       [\[     :_* :Body '(* [:_* :Body])  :_* \]]
    :Either         [\( \|  :_  :Body '(* [:_* :Body])  :_* \)]
    :ZeroOrMore     [\( \*  :_  :Body                   :_* \)]
    :ZeroOrOne      [\( \?  :_  :Body                   :_* \)]
    :AnyNot         [\( \%  :_  '(| :Keyword :Char)     :_* \)]
    :Char           [\\ '(| :TabChar :SpaceChar :NewlineChar (% \space))]
    :TabChar        (pegs "tab")
    :SpaceChar      (pegs "space")
    :NewlineChar    (pegs "newline")
    :ValidKeywordChar (lpegs '| "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789:/*+!_?-")
})

; This needs to be some form of pmap
(defn- either [n g w]
    (let [original (gp w)] ; E.V.I.L.
        (first (keep  #(do  (sp w original) ; E.V.I.L.
                            (pegasus % g w))
                        (rest n)))))

(defn- type-list [n g w]
    (let [t (first n)
          b (second n)]
        (cond   (= t '|) (let [temp (either n g w)] (debug w "Either returning:" temp) temp)
                (= t '*) (list (doall (take-while   #(if (keyword? b) (b %) %) ; When it's not keyword?, it's list? or vector? or char?
                                                    (repeatedly #(pegasus b g w)))))
                (= t '?) (pegasus b g w)
                            ; If we succeed, then we fail - that's the point of AnyNot... If we fail, then we accept the current char
                (= t '%) (let [c (c w)] (if (pegasus b g w) nil (do (debug w "AnyNot MATCH:" (pr-str b)) (m w) c) )))))

(defn- try-char [n w]
    (if (= n (c w))
        (do
            (debug w (str "MATCH: '" (pr-str n) "' with '" (c w) "'"))
            (m w))
        nil))

(defn- peg-vec [n g w]
    (loop [remaining    n
           result       []]
        (if (empty? remaining)
            result
            (let [temp (pegasus (first remaining) g w)]
                (if temp
                    (recur  (rest remaining)
                            (conj result temp))
                    nil)))))

(defn- p [w s n] (debug w s (pr-str n)))

(defn pegasus [n g w]
    (in w)
    (let [result (cond
                    (keyword? n)(do (p w "k:" n)    (let [temp (pegasus (n g) g w)] (if temp {n temp} nil)))
                    (vector? n) (do #_(p w "v:" n)  (peg-vec n g w))
                    (list? n)   (do #_(p w "l:" n)  (type-list n g w))
                    (char? n)   (do #_(p w "c:" n)  (try-char n w))
                    true        (throw (Error. (str "Unknown type: " n))))]
    (de w)
    result))

(def grammar-ast (pr-str
'{:Grammar
 [\{
  {:_* (())}
  {:Rule
   [{:Keyword [\: {:ValidKeywordChar \_} (({:ValidKeywordChar \*}))]}
    {:_ [{:Whitespace \space} (())]}
    {:Body
     {:Grouping
      {:ZeroOrMore
       [\(
        \*
        {:_ [{:Whitespace \space} (())]}
        {:Body
         {:Keyword
          [\:
           {:ValidKeywordChar \W}
           (({:ValidKeywordChar \h}
             {:ValidKeywordChar \i}
             {:ValidKeywordChar \t}
             {:ValidKeywordChar \e}
             {:ValidKeywordChar \s}
             {:ValidKeywordChar \p}
             {:ValidKeywordChar \a}
             {:ValidKeywordChar \c}
             {:ValidKeywordChar \e}))]}}
        {:_* (())}
        \)]}}}]}
  (([{:_ [{:Whitespace \,} (({:Whitespace \space}))]}
     {:Rule
      [{:Keyword
        [\:
         {:ValidKeywordChar \Z}
         (({:ValidKeywordChar \e}
           {:ValidKeywordChar \r}
           {:ValidKeywordChar \o}
           {:ValidKeywordChar \O}
           {:ValidKeywordChar \r}
           {:ValidKeywordChar \M}
           {:ValidKeywordChar \o}
           {:ValidKeywordChar \r}
           {:ValidKeywordChar \e}))]}
       {:_ [{:Whitespace \space} (())]}
       {:Body
        {:Grouping
         {:Sequence
          [\[
           {:_* (())}
           {:Body {:Char [\\ \(]}}
           (([{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \*]}}]
             [{:_* (({:Whitespace \space}))}
              {:Body {:Keyword [\: {:ValidKeywordChar \_} (())]}}]
             [{:_* (({:Whitespace \space}))}
              {:Body
               {:Keyword
                [\:
                 {:ValidKeywordChar \B}
                 (({:ValidKeywordChar \o}
                   {:ValidKeywordChar \d}
                   {:ValidKeywordChar \y}))]}}]
             [{:_* (({:Whitespace \space}))}
              {:Body
               {:Keyword
                [\:
                 {:ValidKeywordChar \_}
                 (({:ValidKeywordChar \*}))]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \)]}}]))
           {:_* (())}
           \]]}}}]}]
    [{:_ [{:Whitespace \,} (({:Whitespace \space}))]}
     {:Rule
      [{:Keyword
        [\:
         {:ValidKeywordChar \B}
         (({:ValidKeywordChar \o}
           {:ValidKeywordChar \d}
           {:ValidKeywordChar \y}))]}
       {:_ [{:Whitespace \space} (())]}
       {:Body
        {:Grouping
         {:Either
          [\(
           \|
           {:_ [{:Whitespace \space} (())]}
           {:Body
            {:Keyword
             [\:
              {:ValidKeywordChar \K}
              (({:ValidKeywordChar \e}
                {:ValidKeywordChar \y}
                {:ValidKeywordChar \w}
                {:ValidKeywordChar \o}
                {:ValidKeywordChar \r}
                {:ValidKeywordChar \d}))]}}
           (([{:_* (({:Whitespace \space}))}
              {:Body
               {:Keyword
                [\:
                 {:ValidKeywordChar \G}
                 (({:ValidKeywordChar \r}
                   {:ValidKeywordChar \o}
                   {:ValidKeywordChar \u}
                   {:ValidKeywordChar \p}
                   {:ValidKeywordChar \i}
                   {:ValidKeywordChar \n}
                   {:ValidKeywordChar \g}))]}}]
             [{:_* (({:Whitespace \space}))}
              {:Body
               {:Keyword
                [\:
                 {:ValidKeywordChar \C}
                 (({:ValidKeywordChar \h}
                   {:ValidKeywordChar \a}
                   {:ValidKeywordChar \r}))]}}]))
           {:_* (())}
           \)]}}}]}]
    [{:_ [{:Whitespace \,} (({:Whitespace \space}))]}
     {:Rule
      [{:Keyword
        [\:
         {:ValidKeywordChar \S}
         (({:ValidKeywordChar \p}
           {:ValidKeywordChar \a}
           {:ValidKeywordChar \c}
           {:ValidKeywordChar \e}
           {:ValidKeywordChar \C}
           {:ValidKeywordChar \h}
           {:ValidKeywordChar \a}
           {:ValidKeywordChar \r}))]}
       {:_ [{:Whitespace \space} (())]}
       {:Body
        {:Grouping
         {:Sequence
          [\[
           {:_* (())}
           {:Body {:Char [\\ \s]}}
           (([{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \p]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \a]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \c]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \e]}}]))
           {:_* (())}
           \]]}}}]}]
    [{:_ [{:Whitespace \,} (({:Whitespace \space}))]}
     {:Rule
      [{:Keyword [\: {:ValidKeywordChar \_} (())]}
       {:_ [{:Whitespace \space} (())]}
       {:Body
        {:Grouping
         {:Sequence
          [\[
           {:_* (())}
           {:Body
            {:Keyword
             [\:
              {:ValidKeywordChar \W}
              (({:ValidKeywordChar \h}
                {:ValidKeywordChar \i}
                {:ValidKeywordChar \t}
                {:ValidKeywordChar \e}
                {:ValidKeywordChar \s}
                {:ValidKeywordChar \p}
                {:ValidKeywordChar \a}
                {:ValidKeywordChar \c}
                {:ValidKeywordChar \e}))]}}
           (([{:_* (({:Whitespace \space}))}
              {:Body
               {:Grouping
                {:ZeroOrMore
                 [\(
                  \*
                  {:_ [{:Whitespace \space} (())]}
                  {:Body
                   {:Keyword
                    [\:
                     {:ValidKeywordChar \W}
                     (({:ValidKeywordChar \h}
                       {:ValidKeywordChar \i}
                       {:ValidKeywordChar \t}
                       {:ValidKeywordChar \e}
                       {:ValidKeywordChar \s}
                       {:ValidKeywordChar \p}
                       {:ValidKeywordChar \a}
                       {:ValidKeywordChar \c}
                       {:ValidKeywordChar \e}))]}}
                  {:_* (())}
                  \)]}}}]))
           {:_* (())}
           \]]}}}]}]
    [{:_ [{:Whitespace \,} (({:Whitespace \space}))]}
     {:Rule
      [{:Keyword
        [\:
         {:ValidKeywordChar \S}
         (({:ValidKeywordChar \e}
           {:ValidKeywordChar \q}
           {:ValidKeywordChar \u}
           {:ValidKeywordChar \e}
           {:ValidKeywordChar \n}
           {:ValidKeywordChar \c}
           {:ValidKeywordChar \e}))]}
       {:_ [{:Whitespace \space} (())]}
       {:Body
        {:Grouping
         {:Sequence
          [\[
           {:_* (())}
           {:Body {:Char [\\ \[]}}
           (([{:_* (({:Whitespace \space}))}
              {:Body
               {:Keyword
                [\:
                 {:ValidKeywordChar \_}
                 (({:ValidKeywordChar \*}))]}}]
             [{:_* (({:Whitespace \space}))}
              {:Body
               {:Keyword
                [\:
                 {:ValidKeywordChar \B}
                 (({:ValidKeywordChar \o}
                   {:ValidKeywordChar \d}
                   {:ValidKeywordChar \y}))]}}]
             [{:_* (({:Whitespace \space}))}
              {:Body
               {:Grouping
                {:ZeroOrMore
                 [\(
                  \*
                  {:_ [{:Whitespace \space} (())]}
                  {:Body
                   {:Grouping
                    {:Sequence
                     [\[
                      {:_* (())}
                      {:Body
                       {:Keyword
                        [\:
                         {:ValidKeywordChar \_}
                         (({:ValidKeywordChar \*}))]}}
                      (([{:_* (({:Whitespace \space}))}
                         {:Body
                          {:Keyword
                           [\:
                            {:ValidKeywordChar \B}
                            (({:ValidKeywordChar \o}
                              {:ValidKeywordChar \d}
                              {:ValidKeywordChar \y}))]}}]))
                      {:_* (())}
                      \]]}}}
                  {:_* (())}
                  \)]}}}]
             [{:_* (({:Whitespace \space}))}
              {:Body
               {:Keyword
                [\:
                 {:ValidKeywordChar \_}
                 (({:ValidKeywordChar \*}))]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \]]}}]))
           {:_* (())}
           \]]}}}]}]
    [{:_ [{:Whitespace \,} (({:Whitespace \space}))]}
     {:Rule
      [{:Keyword
        [\:
         {:ValidKeywordChar \V}
         (({:ValidKeywordChar \a}
           {:ValidKeywordChar \l}
           {:ValidKeywordChar \i}
           {:ValidKeywordChar \d}
           {:ValidKeywordChar \K}
           {:ValidKeywordChar \e}
           {:ValidKeywordChar \y}
           {:ValidKeywordChar \w}
           {:ValidKeywordChar \o}
           {:ValidKeywordChar \r}
           {:ValidKeywordChar \d}
           {:ValidKeywordChar \C}
           {:ValidKeywordChar \h}
           {:ValidKeywordChar \a}
           {:ValidKeywordChar \r}))]}
       {:_ [{:Whitespace \space} (())]}
       {:Body
        {:Grouping
         {:Either
          [\(
           \|
           {:_ [{:Whitespace \space} (())]}
           {:Body {:Char [\\ \A]}}
           (([{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \B]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \C]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \D]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \E]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \F]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \G]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \H]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \I]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \J]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \K]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \L]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \M]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \N]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \O]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \P]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \Q]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \R]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \S]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \T]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \U]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \V]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \W]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \X]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \Y]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \Z]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \a]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \b]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \c]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \d]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \e]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \f]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \g]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \h]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \i]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \j]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \k]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \l]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \m]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \n]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \o]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \p]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \q]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \r]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \s]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \t]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \u]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \v]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \w]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \x]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \y]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \z]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \0]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \1]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \2]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \3]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \4]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \5]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \6]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \7]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \8]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \9]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \:]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \/]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \*]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \+]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \!]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \_]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \?]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \-]}}]))
           {:_* (())}
           \)]}}}]}]
    [{:_ [{:Whitespace \,} (({:Whitespace \space}))]}
     {:Rule
      [{:Keyword
        [\:
         {:ValidKeywordChar \A}
         (({:ValidKeywordChar \n}
           {:ValidKeywordChar \y}
           {:ValidKeywordChar \N}
           {:ValidKeywordChar \o}
           {:ValidKeywordChar \t}))]}
       {:_ [{:Whitespace \space} (())]}
       {:Body
        {:Grouping
         {:Sequence
          [\[
           {:_* (())}
           {:Body {:Char [\\ \(]}}
           (([{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \%]}}]
             [{:_* (({:Whitespace \space}))}
              {:Body {:Keyword [\: {:ValidKeywordChar \_} (())]}}]
             [{:_* (({:Whitespace \space}))}
              {:Body
               {:Grouping
                {:Either
                 [\(
                  \|
                  {:_ [{:Whitespace \space} (())]}
                  {:Body
                   {:Keyword
                    [\:
                     {:ValidKeywordChar \K}
                     (({:ValidKeywordChar \e}
                       {:ValidKeywordChar \y}
                       {:ValidKeywordChar \w}
                       {:ValidKeywordChar \o}
                       {:ValidKeywordChar \r}
                       {:ValidKeywordChar \d}))]}}
                  (([{:_* (({:Whitespace \space}))}
                     {:Body
                      {:Keyword
                       [\:
                        {:ValidKeywordChar \C}
                        (({:ValidKeywordChar \h}
                          {:ValidKeywordChar \a}
                          {:ValidKeywordChar \r}))]}}]))
                  {:_* (())}
                  \)]}}}]
             [{:_* (({:Whitespace \space}))}
              {:Body
               {:Keyword
                [\:
                 {:ValidKeywordChar \_}
                 (({:ValidKeywordChar \*}))]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \)]}}]))
           {:_* (())}
           \]]}}}]}]
    [{:_ [{:Whitespace \,} (({:Whitespace \space}))]}
     {:Rule
      [{:Keyword
        [\:
         {:ValidKeywordChar \E}
         (({:ValidKeywordChar \i}
           {:ValidKeywordChar \t}
           {:ValidKeywordChar \h}
           {:ValidKeywordChar \e}
           {:ValidKeywordChar \r}))]}
       {:_ [{:Whitespace \space} (())]}
       {:Body
        {:Grouping
         {:Sequence
          [\[
           {:_* (())}
           {:Body {:Char [\\ \(]}}
           (([{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \|]}}]
             [{:_* (({:Whitespace \space}))}
              {:Body {:Keyword [\: {:ValidKeywordChar \_} (())]}}]
             [{:_* (({:Whitespace \space}))}
              {:Body
               {:Keyword
                [\:
                 {:ValidKeywordChar \B}
                 (({:ValidKeywordChar \o}
                   {:ValidKeywordChar \d}
                   {:ValidKeywordChar \y}))]}}]
             [{:_* (({:Whitespace \space}))}
              {:Body
               {:Grouping
                {:ZeroOrMore
                 [\(
                  \*
                  {:_ [{:Whitespace \space} (())]}
                  {:Body
                   {:Grouping
                    {:Sequence
                     [\[
                      {:_* (())}
                      {:Body
                       {:Keyword
                        [\:
                         {:ValidKeywordChar \_}
                         (({:ValidKeywordChar \*}))]}}
                      (([{:_* (({:Whitespace \space}))}
                         {:Body
                          {:Keyword
                           [\:
                            {:ValidKeywordChar \B}
                            (({:ValidKeywordChar \o}
                              {:ValidKeywordChar \d}
                              {:ValidKeywordChar \y}))]}}]))
                      {:_* (())}
                      \]]}}}
                  {:_* (())}
                  \)]}}}]
             [{:_* (({:Whitespace \space}))}
              {:Body
               {:Keyword
                [\:
                 {:ValidKeywordChar \_}
                 (({:ValidKeywordChar \*}))]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \)]}}]))
           {:_* (())}
           \]]}}}]}]
    [{:_ [{:Whitespace \,} (({:Whitespace \space}))]}
     {:Rule
      [{:Keyword
        [\:
         {:ValidKeywordChar \T}
         (({:ValidKeywordChar \a}
           {:ValidKeywordChar \b}
           {:ValidKeywordChar \C}
           {:ValidKeywordChar \h}
           {:ValidKeywordChar \a}
           {:ValidKeywordChar \r}))]}
       {:_ [{:Whitespace \space} (())]}
       {:Body
        {:Grouping
         {:Sequence
          [\[
           {:_* (())}
           {:Body {:Char [\\ \t]}}
           (([{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \a]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \b]}}]))
           {:_* (())}
           \]]}}}]}]
    [{:_ [{:Whitespace \,} (({:Whitespace \space}))]}
     {:Rule
      [{:Keyword
        [\:
         {:ValidKeywordChar \N}
         (({:ValidKeywordChar \e}
           {:ValidKeywordChar \w}
           {:ValidKeywordChar \l}
           {:ValidKeywordChar \i}
           {:ValidKeywordChar \n}
           {:ValidKeywordChar \e}
           {:ValidKeywordChar \C}
           {:ValidKeywordChar \h}
           {:ValidKeywordChar \a}
           {:ValidKeywordChar \r}))]}
       {:_ [{:Whitespace \space} (())]}
       {:Body
        {:Grouping
         {:Sequence
          [\[
           {:_* (())}
           {:Body {:Char [\\ \n]}}
           (([{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \e]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \w]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \l]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \i]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \n]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \e]}}]))
           {:_* (())}
           \]]}}}]}]
    [{:_ [{:Whitespace \,} (({:Whitespace \space}))]}
     {:Rule
      [{:Keyword
        [\:
         {:ValidKeywordChar \R}
         (({:ValidKeywordChar \u}
           {:ValidKeywordChar \l}
           {:ValidKeywordChar \e}))]}
       {:_ [{:Whitespace \space} (())]}
       {:Body
        {:Grouping
         {:Sequence
          [\[
           {:_* (())}
           {:Body
            {:Keyword
             [\:
              {:ValidKeywordChar \K}
              (({:ValidKeywordChar \e}
                {:ValidKeywordChar \y}
                {:ValidKeywordChar \w}
                {:ValidKeywordChar \o}
                {:ValidKeywordChar \r}
                {:ValidKeywordChar \d}))]}}
           (([{:_* (({:Whitespace \space}))}
              {:Body {:Keyword [\: {:ValidKeywordChar \_} (())]}}]
             [{:_* (({:Whitespace \space}))}
              {:Body
               {:Keyword
                [\:
                 {:ValidKeywordChar \B}
                 (({:ValidKeywordChar \o}
                   {:ValidKeywordChar \d}
                   {:ValidKeywordChar \y}))]}}]))
           {:_* (())}
           \]]}}}]}]
    [{:_ [{:Whitespace \,} (({:Whitespace \space}))]}
     {:Rule
      [{:Keyword
        [\:
         {:ValidKeywordChar \W}
         (({:ValidKeywordChar \h}
           {:ValidKeywordChar \i}
           {:ValidKeywordChar \t}
           {:ValidKeywordChar \e}
           {:ValidKeywordChar \s}
           {:ValidKeywordChar \p}
           {:ValidKeywordChar \a}
           {:ValidKeywordChar \c}
           {:ValidKeywordChar \e}))]}
       {:_ [{:Whitespace \space} (())]}
       {:Body
        {:Grouping
         {:Either
          [\(
           \|
           {:_ [{:Whitespace \space} (())]}
           {:Body {:Char [\\ {:SpaceChar [\s \p \a \c \e]}]}}
           (([{:_* (({:Whitespace \space}))}
              {:Body
               {:Char [\\ {:NewlineChar [\n \e \w \l \i \n \e]}]}}]
             [{:_* (({:Whitespace \space}))}
              {:Body {:Char [\\ {:TabChar [\t \a \b]}]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \,]}}]))
           {:_* (())}
           \)]}}}]}]
    [{:_ [{:Whitespace \,} (({:Whitespace \space}))]}
     {:Rule
      [{:Keyword
        [\:
         {:ValidKeywordChar \G}
         (({:ValidKeywordChar \r}
           {:ValidKeywordChar \a}
           {:ValidKeywordChar \m}
           {:ValidKeywordChar \m}
           {:ValidKeywordChar \a}
           {:ValidKeywordChar \r}))]}
       {:_ [{:Whitespace \space} (())]}
       {:Body
        {:Grouping
         {:Sequence
          [\[
           {:_* (())}
           {:Body {:Char [\\ \{]}}
           (([{:_* (({:Whitespace \space}))}
              {:Body
               {:Keyword
                [\:
                 {:ValidKeywordChar \_}
                 (({:ValidKeywordChar \*}))]}}]
             [{:_* (({:Whitespace \space}))}
              {:Body
               {:Keyword
                [\:
                 {:ValidKeywordChar \R}
                 (({:ValidKeywordChar \u}
                   {:ValidKeywordChar \l}
                   {:ValidKeywordChar \e}))]}}]
             [{:_* (({:Whitespace \space}))}
              {:Body
               {:Grouping
                {:ZeroOrMore
                 [\(
                  \*
                  {:_ [{:Whitespace \space} (())]}
                  {:Body
                   {:Grouping
                    {:Sequence
                     [\[
                      {:_* (())}
                      {:Body
                       {:Keyword [\: {:ValidKeywordChar \_} (())]}}
                      (([{:_* (({:Whitespace \space}))}
                         {:Body
                          {:Keyword
                           [\:
                            {:ValidKeywordChar \R}
                            (({:ValidKeywordChar \u}
                              {:ValidKeywordChar \l}
                              {:ValidKeywordChar \e}))]}}]))
                      {:_* (())}
                      \]]}}}
                  {:_* (())}
                  \)]}}}]
             [{:_* (({:Whitespace \space}))}
              {:Body
               {:Keyword
                [\:
                 {:ValidKeywordChar \_}
                 (({:ValidKeywordChar \*}))]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \}]}}]))
           {:_* (())}
           \]]}}}]}]
    [{:_ [{:Whitespace \,} (({:Whitespace \space}))]}
     {:Rule
      [{:Keyword
        [\:
         {:ValidKeywordChar \C}
         (({:ValidKeywordChar \h}
           {:ValidKeywordChar \a}
           {:ValidKeywordChar \r}))]}
       {:_ [{:Whitespace \space} (())]}
       {:Body
        {:Grouping
         {:Sequence
          [\[
           {:_* (())}
           {:Body {:Char [\\ \\]}}
           (([{:_* (({:Whitespace \space}))}
              {:Body
               {:Grouping
                {:Either
                 [\(
                  \|
                  {:_ [{:Whitespace \space} (())]}
                  {:Body
                   {:Keyword
                    [\:
                     {:ValidKeywordChar \T}
                     (({:ValidKeywordChar \a}
                       {:ValidKeywordChar \b}
                       {:ValidKeywordChar \C}
                       {:ValidKeywordChar \h}
                       {:ValidKeywordChar \a}
                       {:ValidKeywordChar \r}))]}}
                  (([{:_* (({:Whitespace \space}))}
                     {:Body
                      {:Keyword
                       [\:
                        {:ValidKeywordChar \S}
                        (({:ValidKeywordChar \p}
                          {:ValidKeywordChar \a}
                          {:ValidKeywordChar \c}
                          {:ValidKeywordChar \e}
                          {:ValidKeywordChar \C}
                          {:ValidKeywordChar \h}
                          {:ValidKeywordChar \a}
                          {:ValidKeywordChar \r}))]}}]
                    [{:_* (({:Whitespace \space}))}
                     {:Body
                      {:Keyword
                       [\:
                        {:ValidKeywordChar \N}
                        (({:ValidKeywordChar \e}
                          {:ValidKeywordChar \w}
                          {:ValidKeywordChar \l}
                          {:ValidKeywordChar \i}
                          {:ValidKeywordChar \n}
                          {:ValidKeywordChar \e}
                          {:ValidKeywordChar \C}
                          {:ValidKeywordChar \h}
                          {:ValidKeywordChar \a}
                          {:ValidKeywordChar \r}))]}}]
                    [{:_* (({:Whitespace \space}))}
                     {:Body
                      {:Grouping
                       {:AnyNot
                        [\(
                         \%
                         {:_ [{:Whitespace \space} (())]}
                         {:Char [\\ {:SpaceChar [\s \p \a \c \e]}]}
                         {:_* (())}
                         \)]}}}]))
                  {:_* (())}
                  \)]}}}]))
           {:_* (())}
           \]]}}}]}]
    [{:_ [{:Whitespace \,} (({:Whitespace \space}))]}
     {:Rule
      [{:Keyword
        [\:
         {:ValidKeywordChar \G}
         (({:ValidKeywordChar \r}
           {:ValidKeywordChar \o}
           {:ValidKeywordChar \u}
           {:ValidKeywordChar \p}
           {:ValidKeywordChar \i}
           {:ValidKeywordChar \n}
           {:ValidKeywordChar \g}))]}
       {:_ [{:Whitespace \space} (())]}
       {:Body
        {:Grouping
         {:Either
          [\(
           \|
           {:_ [{:Whitespace \space} (())]}
           {:Body
            {:Keyword
             [\:
              {:ValidKeywordChar \S}
              (({:ValidKeywordChar \e}
                {:ValidKeywordChar \q}
                {:ValidKeywordChar \u}
                {:ValidKeywordChar \e}
                {:ValidKeywordChar \n}
                {:ValidKeywordChar \c}
                {:ValidKeywordChar \e}))]}}
           (([{:_* (({:Whitespace \space}))}
              {:Body
               {:Keyword
                [\:
                 {:ValidKeywordChar \E}
                 (({:ValidKeywordChar \i}
                   {:ValidKeywordChar \t}
                   {:ValidKeywordChar \h}
                   {:ValidKeywordChar \e}
                   {:ValidKeywordChar \r}))]}}]
             [{:_* (({:Whitespace \space}))}
              {:Body
               {:Keyword
                [\:
                 {:ValidKeywordChar \Z}
                 (({:ValidKeywordChar \e}
                   {:ValidKeywordChar \r}
                   {:ValidKeywordChar \o}
                   {:ValidKeywordChar \O}
                   {:ValidKeywordChar \r}
                   {:ValidKeywordChar \M}
                   {:ValidKeywordChar \o}
                   {:ValidKeywordChar \r}
                   {:ValidKeywordChar \e}))]}}]
             [{:_* (({:Whitespace \space}))}
              {:Body
               {:Keyword
                [\:
                 {:ValidKeywordChar \Z}
                 (({:ValidKeywordChar \e}
                   {:ValidKeywordChar \r}
                   {:ValidKeywordChar \o}
                   {:ValidKeywordChar \O}
                   {:ValidKeywordChar \r}
                   {:ValidKeywordChar \O}
                   {:ValidKeywordChar \n}
                   {:ValidKeywordChar \e}))]}}]
             [{:_* (({:Whitespace \space}))}
              {:Body
               {:Keyword
                [\:
                 {:ValidKeywordChar \A}
                 (({:ValidKeywordChar \n}
                   {:ValidKeywordChar \y}
                   {:ValidKeywordChar \N}
                   {:ValidKeywordChar \o}
                   {:ValidKeywordChar \t}))]}}]))
           {:_* (())}
           \)]}}}]}]
    [{:_ [{:Whitespace \,} (({:Whitespace \space}))]}
     {:Rule
      [{:Keyword
        [\:
         {:ValidKeywordChar \K}
         (({:ValidKeywordChar \e}
           {:ValidKeywordChar \y}
           {:ValidKeywordChar \w}
           {:ValidKeywordChar \o}
           {:ValidKeywordChar \r}
           {:ValidKeywordChar \d}))]}
       {:_ [{:Whitespace \space} (())]}
       {:Body
        {:Grouping
         {:Sequence
          [\[
           {:_* (())}
           {:Body {:Char [\\ \:]}}
           (([{:_* (({:Whitespace \space}))}
              {:Body
               {:Keyword
                [\:
                 {:ValidKeywordChar \V}
                 (({:ValidKeywordChar \a}
                   {:ValidKeywordChar \l}
                   {:ValidKeywordChar \i}
                   {:ValidKeywordChar \d}
                   {:ValidKeywordChar \K}
                   {:ValidKeywordChar \e}
                   {:ValidKeywordChar \y}
                   {:ValidKeywordChar \w}
                   {:ValidKeywordChar \o}
                   {:ValidKeywordChar \r}
                   {:ValidKeywordChar \d}
                   {:ValidKeywordChar \C}
                   {:ValidKeywordChar \h}
                   {:ValidKeywordChar \a}
                   {:ValidKeywordChar \r}))]}}]
             [{:_* (({:Whitespace \space}))}
              {:Body
               {:Grouping
                {:ZeroOrMore
                 [\(
                  \*
                  {:_ [{:Whitespace \space} (())]}
                  {:Body
                   {:Keyword
                    [\:
                     {:ValidKeywordChar \V}
                     (({:ValidKeywordChar \a}
                       {:ValidKeywordChar \l}
                       {:ValidKeywordChar \i}
                       {:ValidKeywordChar \d}
                       {:ValidKeywordChar \K}
                       {:ValidKeywordChar \e}
                       {:ValidKeywordChar \y}
                       {:ValidKeywordChar \w}
                       {:ValidKeywordChar \o}
                       {:ValidKeywordChar \r}
                       {:ValidKeywordChar \d}
                       {:ValidKeywordChar \C}
                       {:ValidKeywordChar \h}
                       {:ValidKeywordChar \a}
                       {:ValidKeywordChar \r}))]}}
                  {:_* (())}
                  \)]}}}]))
           {:_* (())}
           \]]}}}]}]
    [{:_ [{:Whitespace \,} (({:Whitespace \space}))]}
     {:Rule
      [{:Keyword
        [\:
         {:ValidKeywordChar \Z}
         (({:ValidKeywordChar \e}
           {:ValidKeywordChar \r}
           {:ValidKeywordChar \o}
           {:ValidKeywordChar \O}
           {:ValidKeywordChar \r}
           {:ValidKeywordChar \O}
           {:ValidKeywordChar \n}
           {:ValidKeywordChar \e}))]}
       {:_ [{:Whitespace \space} (())]}
       {:Body
        {:Grouping
         {:Sequence
          [\[
           {:_* (())}
           {:Body {:Char [\\ \(]}}
           (([{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \?]}}]
             [{:_* (({:Whitespace \space}))}
              {:Body {:Keyword [\: {:ValidKeywordChar \_} (())]}}]
             [{:_* (({:Whitespace \space}))}
              {:Body
               {:Keyword
                [\:
                 {:ValidKeywordChar \B}
                 (({:ValidKeywordChar \o}
                   {:ValidKeywordChar \d}
                   {:ValidKeywordChar \y}))]}}]
             [{:_* (({:Whitespace \space}))}
              {:Body
               {:Keyword
                [\:
                 {:ValidKeywordChar \_}
                 (({:ValidKeywordChar \*}))]}}]
             [{:_* (({:Whitespace \space}))} {:Body {:Char [\\ \)]}}]))
           {:_* (())}
           \]]}}}]}]))
  {:_* (())}
  \}]}

))

(defn self-check []
    (let [result (pr-str (pegasus :Grammar grammar-grammar (gen-ps (pr-str grammar-grammar))))]
        (if (= result grammar-ast)
            (println "Good")
            (println "Fail"))))

