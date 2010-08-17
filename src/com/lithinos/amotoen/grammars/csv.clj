;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.grammars.csv)

(def grammar {
    :Start                  :Document
    :_*                     #"^[ \t]*"
    :Document               ['(+ :Line) :$]
    :Line                   [:_* :Value '(* :_* "," :_* :Value) :_*]
    :Value                  '(| ["\""   :DoubleQuotedValue "\""]
                                ["'"    :SingleQuotedValue "'"]
                                :VanillaValue)
        :DoubleQuotedValue  '(* [(! "\"")   (| :DEscapedChar :AnyChar)])
        :SingleQuotedValue  '(* [(! "'")    (| :SEscapedChar :AnyChar)])
        :VanillaValue       '(* [(! ",")    (| :VEscapedChar :AnyChar)])
            :DEscapedChar   ["\\" '(| "\\" "\"")]
            :SEscapedChar   ["\\" '(| "\\" "\'")]
            :VEscapedChar   ["\\" '(| "\\" "\,")]
            :AnyChar        #"^."
})

(defn specified
    ([]                     (specified ","          "\""    "\""))
    ([separator]            (specified separator    "\""    "\""))
    ([separator, wrapper]   (specified separator    wrapper wrapper))
    ([separator, left-wrapper, right-wrapper] {
        :Start                  :Document
        :_*                     #"^[ \t]*"
        :Document               ['(+ :Line) :$]
        :Line                   [:_* :Value '(* :_* separator :_* :Value) :_*]
        :Value                  [left-wrapper :WrappedValue right-wrapper]
            :WrappedValue       '(* [(! right-wrapper) (| :EscapedChar :AnyChar)])
                :EscapedChar    ["\\" '(| "\\" right-wrapper)]
                :AnyChar        #"^."}))
