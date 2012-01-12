;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.markdown)

(def grammar {
    :Start      {:Document  #(let [ast (first (:Document %))]
                                (loop [result           '()
                                       r                ast
                                       in-code-block    false]
                                    (if (not (empty? r))
                                        (let [el    (:LinePart (first r))
                                              k     (first (keys el))
                                              is-span?  (= :Span k)
                                              process   (if is-span?
                                                            (contains? #{:BlankLine} (first (keys (:Span el))))
                                                            (contains? #{:CodeIndent} k))]
                                            (when process
                                                (println (if is-span? (first (keys (:Span el))) k)))
                                            (recur
                                                (cons result (first r))
                                                (rest r)
                                                false))
                                        (reverse result))))}
    :Document   ['(+ :LinePart) :$]
    :LinePart       '(| :EscapedChar :Span :CodeIndent :RegularChar)
    :RegularChar    #"^(?s)."
    :CodeIndent     [:Enter '(| "    " "\t")]
    :EscapedChar                    ["\\" :EscapableChar]
        :EscapableChar              '(| "\\" "`" "*" "_" "{" "}" "[" "]" "(" ")" "#" "+" "-" "." "!")
    ; Done - as long as regexs like :LinkTextOrLabel consume chars like newline
    :Span                           '(| :Link :LiteralAsterisk :LiteralUnderscore :Emphasis :Code :Image :AutomaticLink :BlankLine)
        ; Done
        :LiteralAsterisk            "* "
        :LiteralUnderscore          "_ "
        ; Done
        :Link                       ["[" :LinkTextOrLabel "]" '(| :InlineLink :ReferenceLink)]
            :LinkTextOrLabel        #"^[^]]+"
            :InlineLink             ["(" :LinkURL '(? [" " "\"" :LinkTitle "\""]) ")"]
            :ReferenceLink          '(| :ImplicitRefLink ['(? " ") "[" :LinkLabel "]"])
               :ImplicitRefLink     ['(? " ") "[]"]
        ; Done
        :Emphasis                   '(| :StrongEmphasis :EMEmphasis)
            :StrongEmphasis         '(| :StrongEmphasis** :StrongEmphasis__)
                :StrongEmphasis**   ["**" '(+ [(! "**") #"^(?s)."]) "**"]
                :StrongEmphasis__   ["__" '(+ [(! "__") #"^(?s)."]) "__"]
            :EMEmphasis             '(| :EMEmphasis* :EMEmphasis_)
                :EMEmphasis*        ["*" #"^[^*]+" "*"]
                :EMEmphasis_        ["_" #"^[^_]+" "_"]
        ; Done-ish
        :Code                       '(| :SingleBacktick :DoubleBacktick); :SpacedBacktick)
            :SingleBacktick         ["`" '(? " ") #"^[^`]+" '(? " ") "`"]
            :DoubleBacktick         ["``" '(? " ") '(+ [(! "``") #"^(?s)."]) '(? " ") "``"]
        ; Done
        :Image                      ["!" :Link]
        ; Done
        :AutomaticLink              ["<" :LinkText ">"]
            :LinkText               #"^[^>]+"
        :BlankLine                  [:Enter #"^[ \t]*" :Enter]
    :Enter      (System/getProperty "line.separator")
})



;(def grammar {
;    :Start                          :Document
;    :Enter                          (System/getProperty "line.separator")
;    :Line                           ['(+ (| :EscapedChar :Span #"^.")) '(| :Enter :$)]
;    :Document                       ['(+ (| :BlankLine :Block :Line)) :$]
;    :EscapedChar                    ["\\" :EscapableChar]
;        :EscapableChar              '(| "\\" "`" "*" "_" "{" "}" "[" "]" "(" ")" "#" "+" "-" "." "!")
;    :Span                           '(| :Link :LiteralAsterisk :LiteralUnderscore :Emphasis :Code :Image :AutomaticLink)
;        :LiteralAsterisk            "* "
;        :LiteralUnderscore          "_ "
;        :Link                       ["[" :LinkTextOrLabel "]" '(| :InlineLink :ReferenceLink)]
;            :LinkTextOrLabel        #"^[^]]+"
;            :InlineLink             ["(" :LinkURL '(? [" " "\"" :LinkTitle "\""]) ")"]
;            :ReferenceLink          '(| :ImplicitRefLink ['(? " ") "[" :LinkLabel "]"])
;               :ImplicitRefLink     ['(? " ") "[]"]
;        :Emphasis                   '(| :StrongEmphasis :EMEmphasis)
;            :StrongEmphasis         '(| :StrongEmphasis** :StrongEmphasis__)
;                :StrongEmphasis**   ["**" '(+ [(! "**") #"^(?s)."]) "**"]
;                :StrongEmphasis__   ["__" '(+ [(! "__") #"^(?s)."]) "__"]
;            :EMEmphasis             '(| :EMEmphasis* :EMEmphasis_)
;                :EMEmphasis*        ["*" #"^[^*]+" "*"]
;                :EMEmphasis_        ["_" #"^[^_]+" "_"]
;        :Code                       '(| :SingleBacktick :DoubleBacktick :SpacedBacktick)
;            :SingleBacktick         ["`" '(? " ") #"^[^`]+" '(? " ") "`"]
;            :DoubleBacktick         ["``" '(? " ") '(+ [(! "``") #"^(?s)."]) '(? " ") "``"]
;        :Image                      ["!" :Link]
;        :AutomaticLink              ["<" :LinkText ">"]
;            :LinkText               #"^[^>]+"
;    :Block                          '(| :Paragraph :Header :Blockquote :List :Codeblock :HRule :LinkLabelDefinition)
;        :Paragraph                  ['(+ :LineOfText) '(& (| :BlankLine :$))]
;            :BlankLine              [#"^[ \t]*$" :Enter]
;            :LineOfText             '(| :BreakEndedLine :Line)
;                :BreakEndedLine     ['(& #"^.+[ ][ ]+$") :Line]
;        :Header                     '(| :SetextHeader :ATXHeader)
;            :SetextHeader           [:HeaderText :Enter '(| (+ "=") (+ "-"))]
;            :ATXHeader              '(| :ATXHeader1 :ATXHeader2 :ATXHeader3 :ATXHeader4 :ATXHeader5 :ATXHeader6)
;                :ATXHeader1         ["#"        :HeaderText '(* "#")]
;                :ATXHeader2         ["##"       :HeaderText '(* "#")]
;                :ATXHeader3         ["###"      :HeaderText '(* "#")]
;                :ATXHeader4         ["####"     :HeaderText '(* "#")]
;                :ATXHeader5         ["#####"    :HeaderText '(* "#")]
;                :ATXHeader6         ["######"   :HeaderText '(* "#")]
;        :Blockquote                 [:BlockquoteFirstLine '(* (| :BlockquoteFirstLine :Line))]
;            :BlockquoteFirstLine    ['(+ "> ") :Line]
;        :List                       '(| :OrderedList :UnorderedList)
;            :OrderedList            '(+ [:OrderedMarker     :ListLine])
;            :UnorderedList          '(+ [:UnorderedMarker   :ListLine])
;                :OrderedMarker      [:ListIndent #"^\d+\."          :ListSpace]
;                :UnorderedMarker    [:ListIndent '(| "*" "+" "-")   :ListSpace]
;                :ListIndent         #"^[ ]{0,3}"
;                :ListSpace          '(| #"^[ ]+" "\t")
;                :ListLine           :Line
;        :Codeblock                  [:CodeIndent :Line]
;            :CodeIndent             '(| "    " "\t")
;        :HRule                      '(| #"^(-|- ){3,}" #"^(\*|\* ){3,}" #"^(_|_ ){3,}")
;        :LinkLabelDefinition        [#"^[ ]{0,3}" "[" :LinkLabel "]:" #"^[ \t]+" :LinkLabelURL '(? (| :SingleLineLLDTitle :DblLineLLDTitle))]
;            :LinkLabel              #"^[^]]+"
;            :LinkLabelURL           '(| :AngleLinkLabelURI :LinkURI)
;                :AngleLinkLabelURI  ["<" :LinkURI ">"]
;            :SingleLineLLDTitle     :LLDTitle
;            :DblLineLLDTitle        [#"[ \t]+\n[ \t]+" :LLDTitle]
;                :LLDTitle           '(| :DblQuoteLLDT :SnglQuoteLLDT :ParenLLDT)
;                    :DblQuoteLLDT   ["\""   :LinkTitle "\""]
;                    :SnglQuoteLLDT  ["'"    :LinkTitle "'"]
;                    :ParenLLDT      ["("    :LinkTitle ")"]})

(defn AutomaticLink-to-html [ast]
    (println ast)
    (let [ast (:AutomaticLink ast)]
        ast))

(defn BlankLine-to-html [ast]
    "\n\n")

(defn Span-to-html [ast]
    (let [ast   (:Span ast)
          k     (first (keys ast))]
        (cond
            (= k :BlankLine)        (BlankLine-to-html ast)
            (= k :AutomaticLink)    (AutomaticLink-to-html ast)
            true                    (do (println "\n\nSpan" k) ast))))

(defn RegularChar-to-html [ast]
    (:RegularChar ast))

(defn CodeIndent-to-html [ast]
    ast)

(defn LinePart-to-html [ast]
    (let [ast   (:LinePart ast)
          k     (first (keys ast))]
        (cond
            (= k :RegularChar)  (RegularChar-to-html            ast)
            (= k :Span)         (Span-to-html                   ast)
            (= k :CodeIndent)   (CodeIndent-to-html             ast)
            true                (do (println "\n\nLinePart" k)  ast))))

(defn markdown-to-html [ast]
    (let [ast (first (:Document ast))]
        (reduce (fn [a b] (str a (LinePart-to-html b))) "" ast)))
