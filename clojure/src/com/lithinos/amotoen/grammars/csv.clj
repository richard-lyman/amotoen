;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.grammars.csv
    (:use [com.lithinos.amotoen.core :only [post-process wrap-string]]))

(def grammar {
    :Document               [:Line '(* :Line) :$]
    :Line                   [:_* :Value '(* [:_* \, :_* :Value]) :_* '(* :EndOfLine)]
    :Value                  '(| [\" (* :DoubleQuotedValue) \"]
                                [\' (* :SingleQuotedValue) \']
                                (* :VanillaValue))
        :DoubleQuotedValue  '(| [\\ \"] [\\ \\] (% \"))
        :SingleQuotedValue  '(| [\\ \'] [\\ \\] (% \'))
        :VanillaValue       ['(! :EndOfLine) '(% \,)]
    :_*                     '(* :Whitespace)
    :Whitespace             '(| \space \tab)
    :EndOfLine              '(| \newline \return)
})

(defn specified
    ([]                     (specified \,\          \"    \"))
    ([separator]            (specified separator    \"    \"))
    ([separator, wrapper]   (specified separator    wrapper wrapper))
    ([separator, left-wrapper, right-wrapper] {
        :Document               [:Line '(* :Line) :$]
        :Line                   [:_* :Value (list '* [:_* separator :_* :Value]) :_* '(* :EndOfLine)]
        :Value                  (list '|    [left-wrapper '(* :WrappedValue) right-wrapper]
                                            '(* :VanillaValue))
            :WrappedValue       (list '| [\\ right-wrapper] [\\ \\] (list '% right-wrapper))
            :VanillaValue       ['(! :EndOfLine) (list '% separator)]
        :_*                     '(* :Whitespace)
        :Whitespace             '(| \space \tab)
        :EndOfLine              '(| \newline \return)
}))

(defn ignore [_] "")
(defn value-as-string [l]
    (let [l (if (vector? l) (second l) l)]
        (cond
            (map? l)    (str (first (vals l)))
            true        (apply  str 
                                (map    #(first (vals %))
                                        l)))))
(defn without-guard [v]
    (second v))
(defn line-as-vec [v]
    (let [others (map last (first (rest (rest v))))]
        (reduce (fn [r i] (conj r (first (vals i))))
                []
                (conj others (second v)))))
(defn lines-as-vec [v]
    (let [v (vec (flatten v))]
        (reduce (fn [r i] (if (empty? i) r (conj r (first (vals i)))))
                []
                (butlast v))))

(def #^{:private true} to-clj-fns { :Document       lines-as-vec
                                    :Line           line-as-vec
                                    :Value          value-as-string
                                    :VanillaValue   without-guard
                                    :_*             ignore
                                    ;:EndOfLine     (fn [_] nil)
                                    :Whitespace     ignore })

(defn to-clj
    ([s] (to-clj s grammar))
    ([s g] (post-process :Document g (wrap-string s) to-clj-fns)))

