;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.grammars.json
  (:use [com.lithinos.amotoen.core :only [pegs lpegs]]))

(defn json-control-character [g w]
    false) ; Until the error messages about 'c' are fixed...
;    (let [s #{  \u0000 \u0001 \u0002 \u0003 \u0004 \u0005 \u0006 \u0007 \u0008 \u0009 \u000A \u000B \u000C \u000D \u000E \u000F
;                \u0010 \u0011 \u0012 \u0013 \u0014 \u0015 \u0016 \u0017 \u0018 \u0019 \u001A \u001B \u001C \u001D \u001E \u001F}]
;        (if (contains? s (c w))
;            (m w)
;            nil)))

(def grammar {
    :_*                     '(* (| \newline \return \tab \space))
    :JSONText               [:_* '(| :JSONObject :Array) :_* :$]
    :Value                  (list '| :JSONString :JSONObject :Array (pegs "true") (pegs "false") (pegs "null") :JSONNumber)
; Objects
    :JSONObject             '(| :EmptyObject :ContainingObject)
        :EmptyObject        [\{ :_* \}]
        :ContainingObject   [\{ :_* :Members :_* \}]
    :Members                '(| [:Pair :_* \, :_* :Members] :Pair) ; Nests the structure significantly
    :Pair                   [:JSONString :_* \: :_* :Value]
; Arrays
    :Array                  '(| :EmptyArray :ContainingArray)
        :EmptyArray         [\[ :_* \]]
        :ContainingArray    [\[ :_* :Elements :_* \]]
    :Elements               '(| [:Value :_* \, :_* :Elements] :Value) ; Nests the structure significantly
; Strings
    :JSONString             '(| :EmptyString :ContainingString)
        :EmptyString        [\" \"]
        :ContainingString   [\" :Chars \"]
    :Chars                  [:GuardedChar '(* :GuardedChar)]
        :GuardedChar        ['(! :ControlCharacter) :Char]
    :ControlCharacter       (list 'a json-control-character)
    :Char                   '(| :EscapedChar [(! \") :NonEscapedChar])
        :EscapedChar        [\\ '(|   \" \\ \/ \b \f \n \r \t :Unicode)]
            :Unicode        [\u :HexDigit :HexDigit :HexDigit :HexDigit]
            :HexDigit       (lpegs '| "0123456789ABCDEFabcdef")
        :NonEscapedChar     :. ; This is OK since the only way it's used is with appropriate guards.
; Numbers
    :JSONNumber             '(| [:Int :Frac :Exp] [:Int :Exp] [:Int :Frac] :Int)
        :Int                '(| [\- :Digit1-9 :Digits] [\- :Digit] [:Digit1-9 :Digits] :Digit)
        :Frac               [\. :Digits]
        :Exp                [:ExpLeader :Digits]
            :Digit          (lpegs '| "0123456789")
            :Digit1-9       (lpegs '| "123456789")
            :Digits         [:Digit '(* :Digits)]
            :ExpLeader      ['(| \e \E) '(* (| \+ \-))]
})
