;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.grammars.minimark
    (:use [com.lithinos.amotoen.core :only [ls post-process wrap-string]]))

(defn containing [s b e] [s b (list '* b) e])

(defn delimited
    ([m] (delimited m m))
    ([b e] (delimited b e e))
    ([s b e] (containing s (list '% b) e)))

(defn delimited-body [d b] (delimited d b d))

(defn a-frequency-ordered-char [] ; Semi-frequency-ordered
    (ls '| "etaoinsrh,.?bcdfgjklmpquvwxyz023456789~`@#$%&*()+}]|:;<>/\""))

(defn one-or-more [b] [b (list '* b)])

(defn one-or-more-not [b] [(list '% b) (list '* (list '% b))])

(def grammar {
    :Content '(* (| :SafeChar :Markup :UnsafeChar))
    :Markup '(| :HRule :MDash :List :SS :U :B :I :Href :Pre :H4 :H3 :H2 :H1)
        :HRule "----"
        :MDash "---"
        :List '(| :OrderedList :UnorderedList)
            :OrderedList    (one-or-more ["1. " :ListBody])
            :UnorderedList  (one-or-more ["-- " :ListBody])
                :ListBody [:ListContent \newline '(* (| \newline \space \tab))]
                :ListContent '(* (| :ListSafeChar :SS :U :B :I :Href :Pre (% \newline)))
        :SS (delimited \^)
        :H4 (delimited-body "====" \=)
        :H3 (delimited-body "===" \=)
        :H2 (delimited-body "==" \=)
        :H1 (delimited-body "=" \=);
        :B (delimited "'''");
        :I (delimited "''");
        :U (delimited "__");
        :Pre (containing "{{{" :PreContent "}}}");
            :PreContent '(| [\! \}] (% [\} \} \}]));
        :Href [\[ (list '| (delimited \[ \]) :HrefExplained) \]];
            :HrefExplained [(one-or-more-not \space) \space (one-or-more-not \])];
    :ListSafeChar (list '| \space :EscapedChar (a-frequency-ordered-char) \tab);
    :SafeChar (list '| :EmptyLine :ListSafeChar \newline);
        :EmptyLine [\newline \newline]
        :EscapedChar [\! :UnsafeChar];
    :UnsafeChar (ls '| "ETAOINSRH1BCDFGJKLMPQUVWXYZ!\\[={_^'-");
})

(defn list-safe-to-html [l] (if (map? l) (first (vals l)) l))

(defn safe-to-html [l] (if (map? l) (first (vals l)) l))

(defn content-to-html [l]
    (cond
        (map? l)    (first (vals l))
        (seq? l)   (apply str (map content-to-html l))
        true        l))

(defn one-or-more-to-str [l]
    (if (char? (second l))
        (apply str l)
        (reduce (fn [a b] (str a b))
                (first l)
                (second l))))

(defn delimited-to-html
    ([l c] (delimited-to-html l c "span"))
    ([l c t] (if (not (nil? l))
                (str "<" t " class='" c "'>" (one-or-more-to-str (butlast (rest l))) "</" t ">"))))

(defn href-to-html [v]
    (if (not (nil? v))
        (if (vector? (second v))
            (let [l (one-or-more-to-str (butlast (rest (second v))))]
                (str "<a href='" l "'>" l "</a>"))
            (let [inside        (first (vals (second v)))
                  link          (one-or-more-to-str (first inside))
                  explanation   (one-or-more-to-str (last inside))]
                (str "<a href='" link "'>" explanation "</a>")))))

(defn pre-to-html[v]
    (if (not (nil? v))
        (let [v (butlast (rest v))]
            (if (= \newline (first (vals (first v))))
                (str    "<div class='pre-block'>"
                        (apply str (map #(first (vals %))
                                        (butlast (second v))))
                        "</div>")
                (str    "<div class='pre-inline'>"             
                        (reduce (fn [a b] (str a (first (vals b))))
                                (first (vals (first v)))
                                (second v))
                        "</div>")))))

(defn list-to-html
    ([v] (list-to-html v "ol"))
    ([v t]
        (if (not (nil? v))
            (str    "<" t ">"
                    (reduce (fn [a b] (if (empty? b) a (str a (str "<li>" (first (vals (last b))) "</li>"))))
                            (str "<li>" (first (vals (last (first v)))) "</li>")
                            (rest v))
                    "</" t ">"))))

(defn list-content-to-html [l]
    (reduce (fn [a b] (str a (if (map? b) (first (vals b)) b)))
            (first l)
            (rest l)))

(def #^{:private true} to-html-fns {:Content        content-to-html
                                    :EscapedChar    #(first (vals (second %)))
                                    :ListSafeChar   list-safe-to-html
                                    :SafeChar       safe-to-html
                                    :MDash          #(if (not (nil? %)) "&mdash;")
                                    :HRule          #(if (not (nil? %)) "<hr />")
                                    :SS             #(delimited-to-html % "superscript")
                                    :U              #(delimited-to-html % "underline")
                                    :B              #(delimited-to-html % "bold")
                                    :I              #(delimited-to-html % "italic")
                                    :H4             #(delimited-to-html % "H4" "div")
                                    :H3             #(delimited-to-html % "H3" "div")
                                    :H2             #(delimited-to-html % "H2" "div")
                                    :H1             #(delimited-to-html % "H1" "div")
                                    :Pre            pre-to-html
                                    :Href           href-to-html
                                    :List           #(first (vals %))
                                    :Markup         #(first (vals %))
                                    :ListBody       #(first (vals (first %)))
                                    :OrderedList    list-to-html
                                    :UnorderedList  #(list-to-html % "ul")
                                    :ListContent    #(list-content-to-html %)
                                    :EmptyLine      #(if (not (nil? %)) "<div class='empty-line' />") })

(defn to-html
    ([s] (to-html s grammar))
    ([s g] (post-process :Content g (wrap-string s) to-html-fns)))
