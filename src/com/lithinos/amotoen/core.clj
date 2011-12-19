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

; Return the AST
(defn pegasus [g i]
    ; Set possible paths to [(path :Start)]
    ; For each char c in i
        ; Ask each path to try to consume c
            ; When a path tries to consume a char
                ; Walk the grammar, from wherever we stopped last
                    ;
                    ;   where we stopped last can be... inside a vector, inside a list
                    ;
                    ; When we hit a keyword, keep walking to the definition of the keywor
                    ; When we hit a vector, walk the first item
                    ; When we hit a terminal, check to see if it is the same as c
                        ; If the terminal is the same as c, return the new path
                        ; If the terminal is not the same as c, return nil
            ; If a path can consume c, it returns a new path representing the ast after consuming c
            ; If a path can not consume c, it returns nil
    ; end
    )

(println (pr-str grammar-grammar))

;(pegasus grammar-grammar (pr-str grammar-grammar))
