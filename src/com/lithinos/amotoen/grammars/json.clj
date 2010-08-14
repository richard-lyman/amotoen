;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.grammars.json)

(def grammar {
    :Start                  :JSONRoot
    :_*                     #"^[\n\r\t ]*"
    :JSONRoot               [ :_* '(* :Value) :_* :$]
    :Value                  '(| :JSONString :JSONNumber :JSONObject :Array "true" "false" "null")
; Objects
    :JSONObject             '(| :EmptyObject :ContainingObject)
        :EmptyObject        ["{" :_* "}"]
        :ContainingObject   ["{" :_* :Members :_* "}"]
    :Members                '(| [:Pair :_* "," :_* :Members] :Pair)
    :Pair                   [:JSONString :_* ":" :_* :Value]
; Arrays
    :Array                  '(| :EmptyArray :ContainingArray)
        :EmptyArray         ["[" :_* "]"]
        :ContainingArray    ["[" :_* :Elements :_* "]"]
    :Elements               '(| [:Value :_* "," :_* :Elements] :Value)
; Strings
    :JSONString             '(| :EmptyString :ContainingString)
        :EmptyString        ["\"" "\""]
        :ContainingString   ["\"" :Chars "\""]
    :Chars                  '(+ [(! :ControlCharacter) :Char])
    :ControlCharacter       #"^[\u0000-\u001F]"
    :Char                   '(| :EscapedChar [(! "\"") :NonEscapedChar])
        :EscapedChar        ["\\" '(|   "\"" "\\" "/" "b" "f" "n" "r" "t" :Unicode)]
            :Unicode        ["u" :HexDigit :HexDigit :HexDigit :HexDigit]
            :HexDigit       #"^[0-9A-Fa-f]"
        :NonEscapedChar     #"^."    ; This is OK since the only way it's used is with appropriate guards.
; Numbers
    :JSONNumber             '(| [:Int :Frac :Exp] [:Int :Exp] [:Int :Frac] :Int)
        :Int                '(| ["-" :Digit1-9 :Digits] ["-" :Digit] [:Digit1-9 :Digits] :Digit)
        :Frac               ["." :Digits]
        :Exp                [:ExpLeader :Digits]
            :Digit          #"^[0-9]"
            :Digit1-9       #"^[1-9]"
            :Digits         #"^[0-9]+"
            :ExpLeader      [#"^[eE]" #"^[+-]*"]
})
