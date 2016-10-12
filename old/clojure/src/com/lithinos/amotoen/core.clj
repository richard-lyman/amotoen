;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.core
    (:import (java.util.regex Pattern)))

(declare to-ast)

(defprotocol Pegable
    "Fulfilling this protocol is required of any input given to to-ast through its 'w' parameter.
     If you'd like to process some String 's', you could use the provided 'wrap-string' function, which
     would reify this protocol around 's' so that you could pass it to to-ast. If you'd like to process
     binary (or any other type of) inputs, you'll need to provide your own reification/implementation."
    (position   [w] [w j]       "Gets and sets the position. This allows for backtracking and debugging.")
    (end?       [w]             "Should return true if there is no more input to consume, false otherwise.")
    (terminal?  [w] [w terminal]"The single paramter version is used to verify that a given grammar is valid
                                 and can be processed. The two parameter version should return true if the 
                                 terminal 'terminal' is a terminal that can be processed by the two paramter 
                                 form of 'move'.")
    (move       [w] [w terminal]"Returns the result of consuming the default size terminal or the terminal 'terminal'. Alters the position accordingly."))

(def #^{:private true
        :dynamic true
        :doc "Bind this to true to 'enable' debug output"}
    *amotoen-debug* false)

(def #^{:private true
        :dynamic true
        :doc "The value this is bound to will be the amount of characters that appear in the debug dump of the input"}
    *amotoen-debug-window* 50)

(defn- debug
    "Sample use can be seen in the four parameter version of to-ast.
     Each debug statement is prefixed with a marker to identify what the line represents.
     There are currently only 4 types of debug prefix markers used.
     '##' is used when a regex is being tested. This will come in a triplet. 
            The first is the string being matched, the second is the regex, and the 
            third is the result of the call to re-find based on that string and regex.
     '++' is used when a terminal matches and the position is about to be moved. The rest of the line is the terminal that matched.
     '->' is used when an element of the grammar is being examined. There are two parts on the
            rest of the line. These two parts are separated by a colon and are, first, a segment
            of the input and, second, the element of the grammar being examined.
     '<-' is used when an element of the grammar is done being examined. The rest of the line is the element of the grammar."
    ([prefix s] (when *amotoen-debug* (println prefix (pr-str s))))
    ([prefix grammar-part w]
        (when *amotoen-debug*
            (let [p (position w)]
                (println    prefix
                            (pr-str (apply
                                        str
                                        (doall
                                            (take   *amotoen-debug-window*
                                                    (repeatedly
                                                        #(try
                                                            (move w)
                                                            (catch Exception e "")))))))
                            ":"
                            (pr-str grammar-part))
                (position w p)))))

(defn wrap-string
    "Reifies Pegable around a string 's', possibly at a given starting point 'j'.
     Characters, Strings, and Regular Expressions are supported as Terminals."
    ([#^String s] (wrap-string s 0))
    ([#^String s j]
        (let [a                 (int-array 1 j)
              size              (count s)
              c                 #(try (.charAt s (aget a 0)) (catch Exception e nil))
              move-x            (fn [condition size terminal]
                                    (if condition
                                        (do (aset-int a 0 (+ size (aget a 0)))
                                            (debug "++" terminal)
                                            terminal)))
              move-character    #(move-x (= %1 (c))                     1           %1)
              move-string       #(move-x (.startsWith s %1 (aget a 0))  (count %1)  %1)
              move-regex        #(let [match (re-find %1 (.substring s (aget a 0)))]
                                    (debug "##" (.substring s (aget a 0)))
                                    (debug "##" %1)
                                    (debug "##" match)
                                    (move-x (not (nil? match))
                                            (count (if (vector? match) (first match) match))
                                            match))]
            (reify Pegable
                (position   [w]             (aget a 0))
                (position   [w k]           (aset-int a 0 k))
                (end?       [w]             (== (aget a 0) size))
                (terminal?  [w]             (or (move w #"^\\(?:(?:tab)|(?:space)|(?:newline)|(?:return)|(?:.))") ; Make sure we don't swallow \"
                                                (move w #"^\"(?:\\|[^\"])++\"")
                                                (move w #"^#\"^(?:\\|\"|[^\"])++\"")))
                (terminal?  [w terminal]    (or (char? terminal)
                                                (instance? String terminal)
                                                (instance? Pattern terminal)))
                (move       [w]             (let [r (c)]
                                                (when (nil? r) (throw (Exception. "Consuming nil")))
                                                (aset-int a 0 (+ 1 (aget a 0)))
                                                r))
                (move       [w terminal]    (cond   (char? terminal)            (move-character terminal)
                                                    (instance? String terminal) (move-string    terminal)
                                                    (instance? Pattern terminal)(move-regex     terminal)
                                                    true                        (throw (Exception. (str "Unsupported terminal: " terminal)))))))))

(defn ls
    "Produces a rule that allows any character in the string 's' as a valid match.
     Since this produces a list, and a succession of 'body' elements, it currently
     only makes sense when called as (ls '| \"abc\") which produces '(| \\a \\b \\c).
     The same effect could be achieved through the regex #\"^[abc]\" but the resulting
     ast would be different."
    [t s] (reverse (into '() (cons t (seq s)))))

(def #^{:private true
        :doc "This grammar is the grammar for Amotoen grammars. It starts at :Grammar."}
    grammar-grammar {
        :_*             '(* :Whitespace)
        :_              [:Whitespace '(* :Whitespace)]
        :Grammar        [\{ :_* :Rule '(* [:_ :Rule]) :_* \}]
        :Rule           [:Keyword :_ :Body]
        :Keyword        [\: '(| :AmotoenSymbol :ProvidedSymbol)]
        :ProvidedSymbol '(| :EndOfInput :AcceptAnything)
        :EndOfInput     \$ ; If the Keyword ':$' is encountered, the wrapped input should be at the end, i.e.: (= true (end? w))
        :AcceptAnything \. ; If the Keyword ':.' is encountered, any character is accepted
        :Body           '(| :Keyword :Terminal :Grouping :NotPredicate :AnyNot :AwareFunction :Function) ; :Terminal should be moved to the end and accept :.
        :Grouping       '(| :Sequence :Either :ZeroOrMore)
        :Sequence       [\[                     :_* :Body '(* [:_* :Body])  :_* \]]
; All elements that start with \( should be under the same Keyword... it'd speed things up a bit - removing the backtracking
;
; ADD THE OPTIONAL OPERATOR '?' (ONE-OR-NONE)
;
        :Either         [\( \|                  :_  :Body '(* [:_* :Body])  :_* \)]
        :OneOrNone      [\( \?                  :_  :Body                   :_* \)]
        :ZeroOrMore     [\( \*                  :_  :Body                   :_* \)]
        :NotPredicate   [\( \!                  :_  :Body                   :_* \)]
        :AnyNot         [\( \%                  :_  :Body                   :_* \)]
        :AwareFunction  [\( \a :_ :CljReaderFn                              :_* \)]
        :Function       [\( \f :_ :CljReaderFn  :_  :Body                   :_* \)]
        :CljReaderFn    [\# \< '(% \>) '(* (% \>)) \>]
        :Whitespace     '(| \space \newline \return \tab \,)
        :Terminal       (list 'a (fn [g w] (terminal? w)))
;        :Terminal       '(| :Char :String :Regex) ; In reality, this is based on what the Pegable input provides as a terminal...
;        :Char                       [\\ (list '| "tab" "space" "newline" "return" '(% \space))]
;        :String                     [\" :Chars \"]
;        :Chars                      [:StringChar '(* :StringChar)]
;        :StringChar                 '(| :EscapedChar [(! \") :.])
;        :EscapedChar                [\\ '(|   \" \\ \/ \b \f \n \r \t)]
        :AmotoenSymbol              [:NonNumericCharacter '(* :AlphanumericCharactersPlus)] ; _Not_ the same as a Clojure Symbol, though it should be a proper subset
        :NonNumericCharacter        (list '% #"^[0123456789]")
        :AlphanumericCharactersPlus #"^[etaoinsrhbcdfgjklmpquvwxyzETAOINSRHBCDFGJKLMPQUVWXYZ0123456789:/*+!_?.-]"}) ; Semi-ordered by frequency - does it matter now?

(defn- either
    "Returns the result of the first element in 'grammar-part' to successfully process something from 'w'."
    [grammar-part g w]
    (let [original (position w)]
        (first
            (keep
                #(do
                    (position w original)
                    (to-ast % g w))
                (rest grammar-part)))))

(defn- any-not
    "This will accept anything that is not 'body'. For instance, '(% :B) would accept 
     any single character that is not whatever matches :B. Success if failure, failure if success.
     It will successfully match any single character if :B fails, and it will fail to match any 
     single character if :B succeeds."
    [body g w]
    (let [p (position w)]
        (if (or (to-ast body g w) (end? w))
            (do (position w p) nil) ; If we succeed (or are at the end), then we fail - that's the point of AnyNot... and rollback
            (move w)))) ; If we fail and aren't at the end, then we accept the current char

(defn- peg-vec
    "Returns the result of calling to-ast on each element in 'grammar-part'.
     If any call fails then nil is returned as the single result of all calls."
    [grammar-part g w]
    (let [p (position w)]
        (loop [remaining    grammar-part
               result       []]
            (if (empty? remaining)
                result
                (let [temp (to-ast (first remaining) g w)]
                    (if temp
                        (recur  (rest remaining)
                                (conj result temp))
                        (do
                            (position w p)
                            nil)))))))

(defn- one-or-none
    "Attempts to call to-ast once. If that result is nil, then the position 
     is reset and an empty list is returned, otherwise the result is returned."
    [body g w]
    (let [p (position w)
          r (to-ast body g w)]
        (if (nil? r)
            (do (position w p) '())
            r)))

(defn- zero-or-more
    "Continues to collect the result of calling (to-ast body g w) until that call returns nil.
     Returns the collected results. If nothing was consumed from 'w', then nil is returned."
    [body g w]
    (let [lastp (ref (position w))] ; This ref has to have a cost...
        (doall
            (take-while
                #(if (== (position w) @lastp)
                    nil
                    (if (keyword? body) (body %) %))
                (repeatedly #(do    (dosync (ref-set lastp (position w)))
                                    (to-ast body g w)))))))

(defn- not-predicate
    "Returns true if (to-ast body g w) doesn't succeed, nil otherwise."
    [body g w]
    (let [p (position w)
          r (nil? (to-ast body g w))]
        (if (or r (end? w))
            (do (position w p) true)
            nil)))

(defn- list-of-one-element
    "Check to find lists containing only a single element.
     Significantly faster than 'count' in the worst-case."
    [r]
    (and
        (seq? r)
        (not (nil? (first r)))
        (nil? (seq (rest r)))))

(defn- typed-list
    "Similar in purpose and result as to-ast, with the expectation that 'grammar-part' is a 'Typed-List' in the grammar."
    [grammar-part g w]
    (let [t                     (first grammar-part)
          rest-of-grammar-part  (second grammar-part)
          result (cond  (= t '|)    (either grammar-part g w)
                        (= t '%)    (any-not        rest-of-grammar-part g w)
                        (= t '?)    (one-or-none    rest-of-grammar-part g w)
                        (= t '*)    (zero-or-more   rest-of-grammar-part g w)
                        (= t '!)    (not-predicate  rest-of-grammar-part g w)
                        (= t 'a)    (rest-of-grammar-part g w)
                        (= t 'f)    (rest-of-grammar-part (to-ast (first (rest (rest grammar-part))) g w)))]
        (if (list-of-one-element result)
            (first result)
            result)))

(defn- peg-keyword
    "Similar in purpose and result as to-ast, with the expectation that 'grammar-part' is a Keyword in the grammar."
    [grammar-part g w]
    (cond
        (= grammar-part :$) (if (end? w) :$ (throw (Error. "Declaration of end without end")))
        (= grammar-part :.) (if (not (end? w)) (move w) (throw (Error. "Attempt to consume any character at end")))
        true                (do (when (nil? (grammar-part g)) (throw (Error. (str "Keyword '" grammar-part "' does not exist in grammar"))))
                                (let [temp (to-ast (grammar-part g) g w)]
                                    (if temp
                                        {grammar-part temp}
                                        nil)))))

(defn- debug-wrap
    ([fn-to-wrap grammar-part]          (debug "-> " grammar-part)     (let [r  (fn-to-wrap)] (debug "<- " grammar-part) r))
    ([fn-to-wrap grammar-part w]        (debug "-> " grammar-part w)   (let [r  (fn-to-wrap)] (debug "<- " grammar-part) r))
    ([fn-to-wrap grammar-part w after]  (debug "-> " grammar-part w)            (fn-to-wrap)))

(defn to-ast
    "Returns the AST resulting from parsing the wrapped input 'w'
     given a grammar definition 'g' and starting at rule 'grammar-part' in 'g'.
     If passed a fourth parameter, it will dump debug statements to the file 'd'."
    ([grammar-part g w d]
        (with-open [dw (clojure.java.io/writer d :append true)] ; Is append the right approach?
            (binding [*amotoen-debug* true
                      *out* dw]
                (to-ast grammar-part g w))))
    ([grammar-part g w]
        (cond
            (keyword?       grammar-part)   (debug-wrap #(peg-keyword   grammar-part g w)   grammar-part)
            (vector?        grammar-part)   (debug-wrap #(peg-vec       grammar-part g w)   grammar-part w)
            (list?          grammar-part)   (debug-wrap #(typed-list    grammar-part g w)   grammar-part w)
            (terminal? w    grammar-part)   (debug-wrap #(move w grammar-part)              grammar-part w false)
            true                            (throw (Error. (str "Unknown type: " grammar-part))))))

(defn with-fns
    "Simplifies attaching 'post-processing' functions to Non-Terminals in the grammar.
     Keys in 'fn-map' should match keys in the grammar 'g'.
     Values in 'fn-map' should be functions accepting the result of having 
     parsed some input according to the related value in 'g'."
    ([g fn-map] (with-fns g fn-map 'f))
    ([g fn-map fn-type]
        (merge-with
            (fn [from-g from-fn-map]
                (list fn-type from-fn-map from-g))
            g
            fn-map)))

(defn post-process
    "Similar to with-fns. Other parameters match to-ast.
     The final result is assumed to be the value of the root map that to-ast would have returned."
    [grammar-keyword g w fn-map]
    (grammar-keyword (to-ast grammar-keyword (with-fns g fn-map) w)))

; It'd be nice if this provided warnings as well...
;   Zero-consumption rules
;   Cyclical rules
;   Full-consumption rules
(defn validate
    "Validate can help identify problems in grammars. 
     Using grammar-grammar as the grammar for valid grammars, 
     it will return nil if the given grammar 'g' is not valid.
     The two parameter form allows passing a debug filename like the
     four parameter form of 'to-ast'."
    ([g] (validate g nil))
    ([g d]
        (let [w     (wrap-string (pr-str g))
              ast   (if (nil? d)
                        (to-ast :Grammar grammar-grammar w)
                        (to-ast :Grammar grammar-grammar w d))
              r     (and    (not (nil? ast))
                            (end? w))]
            [r, ast])))

(defn fastest-theoretical-by-single
    "Finding a lower-bound on any optimizations for speed starts
     by simply reading each character in the string to be parsed 's'.
     The function 'end' is the conditional with 'm' consuming. The
     result of this function is _not_ the fastest you can see in real-world
     use, since the runtime might optimize performance further. This
     should just give an indication of how slow a specific grammar and input
     pairing might be. This also assumes that your grammar will move a single
     position at a time. Some grammars and/or wrappers move faster."
    [s]
    (let [w (wrap-string s)]
        (loop [continue (not (end? w))]
            (when continue
                (position w (inc (position w)))
                (recur (not (end? w)))))))

(defn self-check-fastest
    "This returns the fastest time to read each character of grammar-grammar"
    [] (fastest-theoretical-by-single (pr-str grammar-grammar)))

(defn self-check
    "This ensures that grammar-grammar is a valid grammar.
     It attempts to parse itself. While not every part of valid
     grammars is used in grammar-grammar, it's a nice sanity check."
    [] (validate grammar-grammar))

