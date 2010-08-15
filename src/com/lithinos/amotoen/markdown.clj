;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.markdown)

(def grammar {
; Start
    :Start                          :Document
    :Enter                          (System/getProperty "line.separator")
    :Line                           :???
    :Document                       ['(+ Block) :$]
    :Block                          '(| :Paragraph :Header :Blockquote :List :Codeblock :HRule)
        :Paragraph                  ['(* :BlankLine) '(+ :LineOfText) '(+ :BlankLine)]
            :BlankLine              #"^\s+$"
            :LineOfText             '(| :BreakEndedLine :Line)
                :BreakEndedLine     [:Line #"^[ ][ ]+"]
        :Header                     '(| :SetextHeader :ATXHeader)
            :SetextHeader           [:HeaderText :Enter '(| (+ "=") (+ "-")) :Enter]
            :ATXHeader              '(| :ATXHeader1 :ATXHeader2 :ATXHeader3 :ATXHeader4 :ATXHeader5 :ATXHeader6)
                :ATXHeader1         ["#"        :HeaderText '(* "#") :Enter]
                :ATXHeader2         ["##"       :HeaderText '(* "#") :Enter]
                :ATXHeader3         ["###"      :HeaderText '(* "#") :Enter]
                :ATXHeader4         ["####"     :HeaderText '(* "#") :Enter]
                :ATXHeader5         ["#####"    :HeaderText '(* "#") :Enter]
                :ATXHeader6         ["######"   :HeaderText '(* "#") :Enter]
        :Blockquote                 [:BlockquoteFirstLine '(* (| :BlockquoteFirstLine :Line))]
            :BlockquoteFirstLine    ['(+ "> ") :Line]
        :List                       '(| :OrderedList :UnorderedList)
            :OrderedList            '(+ [:OrderedMarker     :ListLine])
            :UnorderedList          '(+ [:UnorderedMarker   :ListLine])
                :OrderedMarker      [:ListIndent #"^\d+\."          :ListSpace]
                :UnorderedMarker    [:ListIndent '(| "*" "+" "-")   :ListSpace]
                :ListIndent         #"^[ ]{0,3}"
                :ListSpace          '(| #"^[ ]+" "\t")
                :ListLine           :???
        :Codeblock                  [:CodeIndent :Line]
            :CodeIndent             '(| "    " "\t")
        :HRule                      '(| #"^(-|- ){3,}" #"^(\*|\* ){3,}" #"^(_|_ ){3,}")
    :Span                           '(| :Link :Emphasis :Code :Image)
})
