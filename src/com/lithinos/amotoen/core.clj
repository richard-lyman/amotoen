;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.core
    (:use [clojure.set]))

; s - input string
; w - input wrapper
; g - given grammar
; r - result to return
; n - usually a rule or some part of a rule in a grammar

(declare pegasus)

(defprotocol IAmotoen
    "Currently oriented around strings and characters, but could easily be adapted for other approaches."
    (gp         [t]     "Get pos")
    (sp         [t j]   "Set pos")
    (end        [t]     "End of input")
    (c          [t]     "The character at pos")
    (m          [t]     "Returns the 'c' then (inc pos)"))

(defn wrap-string
    "Reifies IAmotoen around a string 's', possibly at a given starting point 'j'.
     The function 'charAt' is part of the mechanism to walk through the string."
    ([#^String s] (wrap-string s 0))
    ([#^String s j] ; 'j' is where we currently are in the input... 'i' was skipped since it was too close to 'i'nput
        (let [a     (int-array 1 j)
              size  (count s)]
            (reify IAmotoen
                (gp     [t]     (aget a 0))
                (sp     [t k]   (aset-int a 0 k))
                (end    [t]     (= (aget a 0) size))
                (c      [t]     (try (.charAt s (aget a 0)) (catch Exception e nil)))
                (m      [t]     (let [r (c t)]
                                    (when (nil? r) (throw (Exception. "Consuming nil")))
                                    (aset-int a 0 (+ 1 (aget a 0)))
                                    r))))))

(defn lpegs
    "Produces a rule that allows any character in the string 's' as a valid match."
    [t s] (reverse (into '() (cons t (seq s)))))

(defn pegs
    "Produces a rule that consumes each character in the string 's' in the order given in 's'."
    [s] (vec (seq s)))

(def #^{:private true
        :doc "This grammar is the grammar for Amotoen grammars. It starts at :Grammar."}
    grammar-grammar {
        :_*             '(* :Whitespace)
        :_              [:Whitespace '(* :Whitespace)]
        :Grammar        [\{ :_* :Rule '(* [:_ :Rule]) :_* \}]
        :Rule           [:Keyword :_ :Body]
        :Keyword        [\: '(| :AmotoenSymbol :ProvidedSymbol)]
        :ProvidedSymbol '(| :EndOfInput :AcceptAnything)
        :EndOfInput     \$ ; If the Keyword ':$' is encountered, the wrapped input should be at the end
        :AcceptAnything \. ; If the Keyword ':.' is encountered, any character is accepted
        :Body           '(| :Keyword :Char :Grouping :NotPredicate :AnyNot :AwareFunction :Function)
        :Grouping       '(| :Sequence :Either :ZeroOrMore)
        :Sequence       [\[                     :_* :Body '(* [:_* :Body])  :_* \]]
        :Either         [\( \|                  :_  :Body '(* [:_* :Body])  :_* \)]
        :NotPredicate   [\( \!                  :_  :Body                   :_* \)]
        :ZeroOrMore     [\( \*                  :_  :Body                   :_* \)]
        :AnyNot         [\( \%                  :_  :Body                   :_* \)]
        :AwareFunction  [\( \a :_ :CljReaderFn                              :_* \)]
        :Function       [\( \f :_ :CljReaderFn  :_  :Body                   :_* \)]
        :CljReaderFn    [\# \< '(% \>) '(* (% \>)) \>]
        :Whitespace                 '(| \space \newline \return \tab \,)
        :Char                       [\\ (list '| (pegs "tab") (pegs "space") (pegs "newline") (pegs "return") '(% \space))]
        :AmotoenSymbol              [:NonNumericCharacter '(* :AlphanumericCharactersPlus)] ; _Not_ the same as a Clojure Symbol, though it should be a proper subset
        :NonNumericCharacter        (list '% (lpegs '| "0123456789"))
        :AlphanumericCharactersPlus (lpegs '| "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789:/*+!-_?.")})

(defn- either
    "Returns the result of the first element in 'n' to successfully process something from 'w'."
    [n g w]
    (let [original (gp w)]
        (first
            (keep
                #(do
                    (sp w original)
                    (pegasus % g w))
                (rest n)))))

(defn- any-not
    "This will accept anything that is not 'b'. For instance, '(% :B) would accept 
     any single character that is not whatever matches :B. Success if failure, failure if success.
     It will successfully match any single character if :B fails, and it will fail to match any 
     single character if :B succeeds."
    [b g w]
    (let [p (gp w)]
        (if (or (pegasus b g w) (end w))
            (do (sp w p) nil) ; If we succeed (or are at the end), then we fail - that's the point of AnyNot... and rollback
            (m w)))) ; If we fail and aren't at the end, then we accept the current char

(defn debug
    "Very very inefficient but useful in some cases.
     Prints out 'i' number of characters from 'w' followed by 'n', 
     and then resets the position in 'w' as if nothing had been consumed."
    [n w i]
    (let [p (gp w)]
        (println ">> " (pr-str (apply str (doall (take  i (repeatedly #(try (m w) (catch Exception e ""))))))) ":" n)
        (sp w p)))

(defn- try-char [n w]
    ;(debug n w 25)
    (if (= n (c w))
        (m w)
        nil))

(defn- peg-vec
    "Returns the result of calling pegasus on each element in 'n'.
     If any call fails then nil is returned as the single result of all calls."
    [n g w]
    (let [p (gp w)]
        (loop [remaining    n
               result       []]
            (if (empty? remaining)
                result
                (let [temp (pegasus (first remaining) g w)]
                    (if temp
                        (recur (rest remaining)
                            (conj result temp))
                        (do
                            (sp w p)
                            nil)))))))

(defn- zero-or-more
    "Continues to collect the result of calling (pegasus b g w) until that call returns nil.
     Returns the collected results. If nothing was consumed from 'w', then nil is returned."
    [b g w]
    (let [lastp (ref (gp w))]
        (doall
            (take-while
                #(if (= (gp w) @lastp)
                    nil
                    (if (keyword? b) (b %) %))
                (repeatedly #(do    (dosync (ref-set lastp (gp w)))
                                    (pegasus b g w)))))))

(defn- not-predicate
    "Returns true if (pegasus b g w) doesn't succeed, nil otherwise."
    [b g w]
    (let [p (gp w)
          r (nil? (pegasus b g w))]
        (if (or r (end w))
            (do (sp w p) true)
            nil)))

(defn- list-of-one-element
    "Check to find lists containing only a single element.
     Significantly faster than 'count' in the worst-case."
    [r]
    (and
        (seq? r)
        (nil? (seq (rest r)))
        (not (nil? (first r)))))

(defn- typed-list
    "Similar in purpose and result as pegasus, with the expectation that n is a 'Typed-List' in the grammar."
    [n g w]
    (let [t (first n)
          b (second n)
          result (cond  (= t '|)    (either n g w)
                        (= t '%)    (any-not b g w)
                        (= t '*)    (zero-or-more b g w)
                        (= t '!)    (not-predicate b g w)
                        (= t 'a)    (b g w)
                        (= t 'f)    (b (pegasus (first (rest (rest n))) g w)))]
        (if (list-of-one-element result)
            (first result)
            result)))

(defn- peg-keyword
    "Similar in purpose and result as pegasus, with the expectation that n is a Keyword in the grammar."
    [n g w]
    (cond
        (= n :$)    (if (end w) :$ (throw (Error. "Declaration of end without end")))
        (= n :.)    (if (not (end w)) (m w) (throw (Error. "Attempt to consume any character at end")))
        true        (do
                        (when (nil? (n g)) (throw (Error. (str "Keyword '" n "' does not exist in grammar"))))
                        (let [temp (pegasus (n g) g w)]
                            (if temp
                                {n temp}
                                nil)))))

(defn pegasus
    "Returns the AST resulting from parsing the wrapped input 'w'
     given a grammar definition 'g' and starting at rule 'n' in 'g'."
    [n g w]
    ;(when (keyword? n) (debug n w 25))
    (cond
        (keyword? n)(peg-keyword n g w)
        (vector? n) (peg-vec n g w)
        (list? n)   (typed-list n g w)
        (char? n)   (try-char n w)
        true        (throw (Error. (str "Unknown type: " n)))))

(defn with-fns
    "Simplifies attaching 'post-processing' functions to Non-Terminals in the grammar.
     Keys in 'fn-map' should match keys in the grammar 'g'.
     Values in 'fn-map' should be functions accepting the result of having 
     parsed some input according to the related value in 'g'."
    ([g fn-map] (with-fns g fn-map 'f))
    ([g fn-map fn-type]
        (merge-with (fn [from-g from-fn-map]
                    (list fn-type from-fn-map from-g))
                g
                fn-map)))

(defn post-process
    "Similar to with-fns. Other parameters match pegasus.
     The final result is assumed to be the value of the root map that pegasus would have returned."
    [n g w fn-map]
    (n (pegasus n (with-fns g fn-map) w)))

(defn validate
    "Validate can help identify problems in grammars. 
     Using grammar-grammar as the grammar for valid grammars, 
     it will return nil if the given grammar 'g' is not valid."
    [g]
    (let [w     (wrap-string (pr-str g))
          ast   (pegasus :Grammar grammar-grammar w)
          r     (and    (not (nil? ast))
                        (end w))]
        [r, ast]))

(defn fastest-theoretical
    "Finding a lower-bound on any optimizations for speed starts
     by simply reading each character in the string to be parsed 'b'.
     The function 'end' is the conditional with 'm' consuming."
    [s]
    (let [w (wrap-string s)]
        (loop [continue (not (end w))]
            (when continue
                (m w)
                (recur (not (end w)))))))

(defn self-check-fastest
    "This returns the fastest time to read each character of grammar-grammar"
    [] (fastest-theoretical (pr-str grammar-grammar)))

(defn self-check
    "This ensures that grammar-grammar is a valid grammar.
     It attempts to parse itself. While not every part of valid
     grammars is used in grammar-grammar, it's a nice sanity check."
    [] (validate grammar-grammar))

