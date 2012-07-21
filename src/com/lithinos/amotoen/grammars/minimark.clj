;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.grammars.minimark
    (:use [com.lithinos.amotoen.core]))

(defn delimited
    ([m] (delimited m m))
    ([s e] [s [(list '% e) (list '* (list '% e))] e]))

(def grammar {
;Bulk
    :Content                [:Element '(* :Element) :$]
    :Element                '(| :_+ :Alphanumeric :Markup :.)
    :Markup                 '(| :HRule :MDash :List :SS :U :H4 :H3 :H2 :H1 :B :I :Href :Pre)
    :Markup-Guard           (list '! (list '| :HRule :MDash :Unordered-List-Marker :Ordered-List-Marker
                                    \^ (pegs "__") (pegs "====") (pegs "===") (pegs "==") \= (pegs "'''") (pegs "''") \[ (pegs "{{{")))
    :Non-Markup             [:Markup-Guard :Any-Char]
    :NM+                    [:Non-Markup '(* :Non-Markup)]
; Whitespace
    :N                      \newline
    :_                      \space
    :_*                     '(* :_)
    :_+                     [:_ '(* :_)]
; Odds-n-Ends
    :Any-Char               '(| :Empty-Line :Escaped-Char :.)
    :Empty-Line             [:N :N]
    :Escaped-Char           [\! (pegs "ABCDEFGHIJKLMNOPQRSTUVWXYZ!\\[=]")]
    :Alphanumeric           (pegs "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789") ; No markup ever starts with an alphanumeric character
;Links
    :Href                   '(| :Href-Straight :Href-Explained)
        :Href-Straight      [\[ :Href-Body \]]
        :Href-Explained     [\[ :Href-Text :_ :Href-Body \]]
            :Href-Body      [:Href-Body-Part '(* :Href-Body-Part)]
            :Href-Body-Part '(% (| \] \space))
            :Href-Text      ['(% \space) '(* (% \space))]
;Lists
    :Unordered-List-Marker  (pegs "-- ")
    :Ordered-List-Marker    (pegs "-. ")
    :List                   '(| :Ordered-List :Unordered-List)
    :Ordered-List           [:_* :Ordered-List-Marker :List-Body '(* [:_* :Ordered-List-Marker :List-Body])]
    :Unordered-List         [:_* :Unordered-List-Marker :List-Body '(* [:_* :Unordered-List-Marker :List-Body])]
        :List-Body          [:List-Chunk '(* :List-Chunk)]
            :List-Chunk     '(| :SS :U :B :I :Href :Pre :.)
;Superscript
    :SS                     [\^ :SS-Body \^]
        :SS-Body            ['(! \^) :SS-Chunk '(* [(! \^) :SS-Chunk])]
        :SS-Chunk           '(| :U :B :I :Href :.)
;Pre
    :Pre                    (delimited (pegs "{{{") (pegs "}}}"))
;Remaining
    :H1                     (delimited \=)
    :H2                     (delimited (pegs "=="))
    :H3                     (delimited (pegs "==="))
    :H4                     (delimited (pegs "===="))
    :B                      (delimited (pegs "'''"))
    :I                      (delimited (pegs "''"))
    :U                      (delimited (pegs "__"))
    :HRule                  (pegs "----")
    :MDash                  (pegs "---")})
