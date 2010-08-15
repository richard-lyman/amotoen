;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.minimark)

(def grammar {
; Start
    :Start                  :Content
; Whitespace
    :N                      "\n"
    :_                      " "
    :_*                     #"^\s*"
; Odds-n-Ends
    :Any-Char               '(| :Empty-Line :Escaped-Char #"(?s)^.")
    :Empty-Line             [:N :N]
    :Escaped-Char           ["!" #"^[A-Z!\[=]"]
    :Alphanumeric           #"^[A-Za-z0-9]" ; No markup ever starts with an alphanumeric character
;Bulk
    :Content                ['(+ (| #"^\s+" :Alphanumeric :Markup #"^.")) :$]
    :Markup                 '(| :HRule :MDash :List :SS :U :H4 :H3 :H2 :H1 :B :I :Href :Pre)
    :Markup-Guard           '(! (| :HRule :MDash :List-Marker "^" "__" "====" "===" "==" "=" "'''" "''" "[" "{{{"))
    :Non-Markup             [:Markup-Guard '(| :Any-Char)]
    :NM+                    '(+ :Non-Markup)
;Links
    :Href                   '(| :Href-Straight :Href-Explained)
        :Href-Straight      ["[" :Href-Body "]"]
        :Href-Explained     ["[" :Href-Text :_ :Href-Body "]"]
            :Href-Body      #"^[^] ]+"
            :Href-Text      #"^[^ ]+"
;Lists
    :List-Marker            '(| "-- " "-. ")
    :List                   '(| :Ordered-List :Unordered-List)
    :Ordered-List           '(+ [:_* "-. " :List-Body])
    :Unordered-List         '(+ [:_* "-- " :List-Body])
        :List-Body          '(+ :List-Chunk)
            :List-Chunk     '(| :SS :U :B :I :Href :Pre #"^.")
;Superscript
    :SS                     ["^" :SS-Body "^"]
        :SS-Body            '(+ [(! "^") :SS-Chunk])
        :SS-Chunk           '(| :U :B :I :Href #"^.")
;Pre
    :Pre                    ["{{{" :Pre-Markup-Body "}}}"]
        :Pre-Markup-Body    '(+ [(! "}}}") :Any-Char])
;Remaining
    :H1                     ["="    #"^[^=]+"                   "="]
    :H2                     ["=="   '(+ [(! "==")    #"^."])    "=="]
    :H3                     ["==="  '(+ [(! "===")   #"^."])    "==="]
    :H4                     ["====" '(+ [(! "====")  #"^."])    "===="]
    :B                      ["'''"  '(+ [(! "'''")   #"^."])    "'''"]
    :I                      ["''"   '(+ [(! "''")    #"^."])    "''"]
    :U                      ["__"   '(+ [(! "__")    #"^."])    "__"]
    :HRule                  "----"
    :MDash                  "---"})
