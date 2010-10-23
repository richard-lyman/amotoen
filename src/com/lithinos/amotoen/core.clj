;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.core
    (:use (com.lithinos.amotoen errors string-wrapper wrapper))
    (:import (java.util.regex Pattern)))

(def #^{:private true} *EOF-mismatch*       "End of grammar failed to match end of input")
(def #^{:private true} *invalid-grammar*    "Invalid grammar")
(defn- lookahead-error      [o] (str "Lookahead failed:" o))
(defn- option-error         [o] (str "Ran out of options: " o))
(defn- consume-error        [o] (str "Check failed: '" o "' can not be consumed"))
(defn- grouping-error       [o] (str "Unsupported grouping type:" o))
(defn- cycling-error        [o] (str "Cycling on:" (pr-str o)))
(defn- unknown-base-error   [o] (str "Unknown base:" o))

(declare process)
(declare grammar-parser)

;
; One of only two public functions
; This takes _quite_ a while... much longer than it should.
;
(defn validate [g] (:Start g) (grammar-parser (wrap-string (pr-str g))))

;
; Levels: 0 wil eventually call 1 which can call functions in 2 which can call functions in 3 which can call functions in 4
;

;
; LEVEL 0 FUNCTION (the only other public function)
;
(defn create-parser
    "Sample use: (println \"Resulting AST:\" ((create-parser grammar) (wrap-string input)))"
    [g & o]
    (let [s (or (= (first o) :validate-grammar) (= (second o) :validate-grammar))
          d (or (= (first o) :debug)            (= (second o) :debug))]
        (fn [#^IWrapper i]
            (reset i)
            (set-debug i d)
            (try
                (:Start (process :Start g i 0 s))
                (catch Error e (do (fail i e) (throw e)))))))

;
; ODDS-N-ENDS FUNCTIONS
;
(defn- pattern? [i] (instance? Pattern i))
(defn- strict-terminal? [i] (string? i))
(defn- terminal? [i] (or (strict-terminal? i) (pattern? i)))

;
; LEVEL 4 FUNCTIONS
;
(defn- process-n [initial base g i l]
    (loop [result initial]
        (try
            (recur (conj result (process base g i l)))
            (catch Error e (cond
                            (is-amotoen-error e)            (reverse result)
                            (is-amotoen-cyclical-error e)   (reverse (rest result))
                            true                            (throw e))))))
(defn- process-lookahead [success base g i l]
    (let [m         (get-mark i)
          result   (try    (process base g i l)
                            success
                            (catch Error e (not success)))]
        (return-to-mark i m)
        (if result
            success
            (throw (amotoen-error (lookahead-error base))))))

;
; LEVEL 3 FUNCTIONS
;
(defn- process* [base g i l] (process-n ()                               base g i l))
(defn- process+ [base g i l] (process-n (conj () (process base g i l))   base g i l))
(defn- process& [base g i l] (process-lookahead true     base g i l))
(defn- process! [base g i l] (process-lookahead false    base g i l))
(defn- process? [base g i l]
    (try
        (process base g i l)
        (catch Error e (if  (is-amotoen-error e)
                            '()
                            (throw e)))))

; Does this work in all cases?
; It expects a grammar (something) like: {:Something '(<+ :Thing) :Thing '(| [(! :Guard) :Any-Char])}
(defn- process<+ [base g i l]
    (let [result (process+ base g i l)]
        (map #(first (vals (second (first (vals %))))) result)))

; Hopefully certain cases can be sped up.
; For cases where each option in the group is marked by a token (some set of characters)
;   we can branch directly to the one (only one) option that will work by testing for the first token
;   and not need to bother testing any other option, since they won't work.
(defn- process| [base-set g i l]
    (let [m (get-mark i)]
        (loop [remaining base-set]
            (let [success (try
                            (process (first remaining) g i l)
                            (catch Error e nil))]
                (if (= nil success)
                    (do
                        (return-to-mark i m)
                        (if (> (count (rest remaining)) 0)
                            (recur (rest remaining))
                            (throw (amotoen-error (option-error base-set)))))
                    success)))))

(defn- processfn [callee base g i l]
    ((eval callee) (process base g i l)))

;
;   LEVEL 2 FUNCTIONS
;
(defn- start-process [terminal base g i l]
    (if (cyclical? i base)
        (do
            (debug i l (str "! " (pr-str base))) 
            (throw (amotoen-cyclical-error (cycling-error base)))))
    (cyclical-track i base)
    (if (and    (not terminal)
                (not (keyword? base)))
        (debug i l (pr-str base)))
    (if (= (first (keys g)) base)
        (debug-with-context i l (pr-str base))))

(defn- process-eof [i]
    (if (end? i)
        :EOF
        (throw (amotoen-error *EOF-mismatch*))))

(defn- process-terminal [base i]
    (if (consume? i base)
        (consume i base) 
        (throw (amotoen-error (consume-error base)))))

(defn- process-grouping [base g i l]
    (let [type (first base)]
        (cond
            (= '*   type) (process*     (second base)               g i l)
            (= '+   type) (process+     (second base)               g i l)
            (= '?   type) (process?     (second base)               g i l)
            (= '&   type) (process&     (second base)               g i l)
            (= '!   type) (process!     (second base)               g i l)
            (= '<+  type) (process<+    (second base)               g i l)
            (= '|   type) (process|     (rest base)                 g i l)
            (= 'fn  type) (processfn    (nth base 1) (nth base 2)   g i l)
            true        (throw (Error. (grouping-error type))))))

(defn- wrap-up-process [terminal result base i l]
    ;(cyclical-pop i)
    (if (and (not terminal) (keyword? base))
        (debug-with-context i l (str "- " (pr-str base))))
    (if (and (not terminal) (keyword? base))
        {base result}
        result))

;
; LEVEL 1 FUNCTION
;
(defn- process
    ([base g i l] (process base g i l false))
    ([base g i l s]
        (if s (try (validate g) (catch Error e (throw (amotoen-error *invalid-grammar*)))))
        (let [n (+ 1 l)
              terminal (terminal? base)]
            (start-process terminal base g i l)
            (try
                (let [result (cond
                                (= :$ base)     (process-eof i)
                                (keyword? base) (do (debug i l base) (process (base g) g i n))
                                (map? base)     (throw (Error. (str "-- MAP IS NOT SUPPORTED -- " base)))   ; TODO
                                (vector? base)  (reduce #(conj %1 (process %2 g i n)) [] base) ; This needs to fail-fast... don't keep reducing if any previous one fails
                                terminal        (process-terminal base i)
                                (list? base)    (process-grouping base g i n)
                                true            (throw (Error. (unknown-base-error base))))]
                    (wrap-up-process terminal result base i l))
                (catch Error e (do (if (not terminal) (debug-with-context i l (str "X " (pr-str base)))) (throw e)))))))


; A Grammar to validate grammars
(def #^{:private true} grammar-grammar {
    :Start              :Expr
    :Expr               [:_* "{" :_* '(+ :Rule) :_* "}" :_* :$]
; Whitespace
    :_*                 #"^[ \n\r\t]*"
    :_                  #"^[ \n\r\t]+"
; Non-Terminals
    :Rule               [:_* :Keyword :_ :Body :_* "," :_*]
    :Keyword            #"^:[A-Za-z0-9:/*+!_?-][A-Za-z0-9:/*+!_?-]*"
    :Body               '(| :Keyword :Grouping :Terminal)
    :Bodies             [:Body '(* [:_* :Body])]
    :Grouping           '(| :Track :Sequence :Option :ZeroOrMore :OneOrMore :ZeroOrOne 
                            :PositiveLookahead :NegativeLookahead :Gather :ByteTest :ByteMaskThenTest
                            :Call)
    :Track              ["{"    :_* :Keyword :_     :Body       :_* "}"]
    :Sequence           ["["    :_*                 :Bodies     :_* "]"]
    :Option             ["(|"   :_                  :Bodies     :_* ")"]
    :ZeroOrMore         ["(*"   :_                  :Body       :_* ")"]
    :OneOrMore          ["(+"   :_                  :Body       :_* ")"]
    :ZeroOrOne          ["(?"   :_                  :Body       :_* ")"]
    :PositiveLookahead  ["(&"   :_                  :Body       :_* ")"]
    :NegativeLookahead  ["(!"   :_                  :Body       :_* ")"] 
    :Gather             ["(<+"  :_                  :Body       :_* ")"]
    :ByteTest           ["(B&"  :_                :ByteNumber   :_* ")"]
    :ByteMaskThenTest   ["(B&"  :_ :ByteNumber :_ :ByteNumber   :_* ")"]
    :Call               ["(fn"  :_* :Callee :_* :Body           :_* ")"]
    :Terminal           '(| :RegEx :DoubleQuotedString :Binary ":$")
; Terminals
    :ByteNumber         '(| :RadixSuppliedNumber :HexNumber :Digit)
        :RadixSuppliedNumber        #"^\d+r\d+"
        :HexNumber                  #"^0x\d+"
        :Digit                      #"^\d+"
    :RegEx              ["#" :DoubleQuotedString]
    :DoubleQuotedString ["\"" '(+ :DoubleQuotedStringContent) "\""]
        :DoubleQuotedStringContent  '(| :EscapedSlash :EscapedDoubleQuote :AnyNotDoubleQuote)
            :EscapedSlash               #"^[\\][\\]"
            :EscapedDoubleQuote         #"^[\\]\""
            :AnyNotDoubleQuote          #"^[^\"]"
    :Binary             ['(| :BinaryGetByte :BinaryGetBit) :BinaryGetSize]
        :BinaryGetByte              "B"
        :BinaryGetBit               "b"
        :BinaryGetSize              #"^\d+"})

; Used in validating grammars
(def #^{:private true} grammar-parser (create-parser grammar-grammar))

