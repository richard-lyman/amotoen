;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.test.core
  (:import (java.io File))
  (:use [com.lithinos.amotoen.core] :reload-all)
  (:use [clojure.test])
  (:use [clojure.pprint])
  (:use [com.lithinos.amotoen.grammars
            [csv        :rename {grammar csv-grammar specified csv-specified}]
            [json       :rename {grammar json-grammar}]
            [minimark   :rename {grammar minimark-grammar}]
            ]))

(defn test-grammars []
    (clojure.java.io/delete-file "debug.txt" true)
    (let [cf #(apply str %)]
        (doseq [g [ {:S \a}
                    {:S "}}}"}
                    {:S (list '% "}}}")}
                    {:S (list '* (list '% "}}}"))}
                    {:S [(list '* (list '% "}}}")) "}}}"]}
                    {:S [(list 'f cf "abcabc")]}
                    {:S [(list 'f cf (list '* (ls '| "abc")))]}
                    {:S [(list 'f cf '(* (| :A :B :C)))] :A \a :B \b :C \c }
                    csv-grammar
                    json-grammar
                    minimark-grammar
                    ]]
            (if (= g csv-grammar)
                (is (first (validate g "debug.txt")))
                (is (first (validate g))))
)))

(defn vectors-reset-pos []
    (let [g {:S [(list '* (list '% "}}}")) "}}}"]}
          i "a}}b}}}"
          r (to-ast :S g (wrap-string i) "something.txt")]
        (is (= '{:S [(\a \} \} \b) "}}}"]} r))))

(defn collapse-ls []
    (let [custom-collapse #(apply str %)
          g {:S [(list 'f custom-collapse (list '* (ls '| "abc")))]}
          i "aabbcc"
          r (to-ast :S g (wrap-string i))]
        (is (= '{:S ["aabbcc"]} r))))

(defn collapse-keywords []
    (let [custom-collapse (fn [r] (apply str (map #(first (vals %)) r)))
          g {:S [(list 'f custom-collapse '(* (| :A :B :C)))] :A \a :B \b :C \c }
          i "aabbcc"
          r (to-ast :S g (wrap-string i))]
        (is (= '{:S ["aabbcc"]} r))))

(defn extract [time-result]
    (Double/parseDouble
        (nth
            (.split
                (last
                    (.split time-result "\n"))
                " ")
            2)))

(do
    (println "Speed tests")
    (let [fastest   (with-out-str (time (self-check-fastest)))
          single    (with-out-str (time (self-check)))
          avg50     (with-out-str (time (doall (take 50 (repeatedly #(self-check))))))]
        (printf "%8.2fms - Single Run (moving faster than a character at a time)\n" (extract single))
        (printf "%8.2fms - Average over 50 runs (moving faster than a character at a time)\n" (/ (extract avg50) 50))
        (printf "%8.2fms - Fastest theoretically possible, when moving a single terminal at a time (in this case, a character at a time)\n" (extract fastest))
        (println "\n (in milliseconds)\n")))

(do
    (println "Corner cases")
    (test-grammars)
    (vectors-reset-pos)
    (collapse-ls)
    (collapse-keywords)
)

(try
    (to-ast :S {:A :B} (wrap-string "fail"))
    (throw (Error. "A useful error should be thrown when a keyword doesn't exist in a grammar"))
    (catch Error e))

(deftest regex []
    (let [i "aabb"
          g {:S #"^a+b+"}
          r (to-ast :S g (wrap-string i))]
        (is (= {:S "aabb"} r))))

(deftest compare-char-string-regex-speed
    (let [i (str (apply str (repeat 100000 "a")) (apply str (repeat 100000 "b")))
          cg {:S ['(* \a) '(* \b)]}
          sg {:S ['(* "a") '(* "b")]}
          rg {:S #"^a*b*"}
          cr (to-ast :S cg (wrap-string i))
          sr (to-ast :S sg (wrap-string i))
          rr (to-ast :S rg (wrap-string i))
          ct ["char" (extract (with-out-str (time (to-ast :S cg (wrap-string i)))))]
          st ["string" (extract (with-out-str (time (to-ast :S sg (wrap-string i)))))]
          rt ["regex" (extract (with-out-str (time (to-ast :S rg (wrap-string i)))))]]
        (println "===")
        (println "In a very contrived example (100k a's followed by 100k b's)...")
        (println (apply str (interpose " is faster than " (map first (sort-by second [ct st rt])))))
        (println "Using the following grammars:")
        (println "\tchar" (pr-str cg))
        (println "\tstring" (pr-str sg))
        (println "\tregex" (pr-str rg))
        (println "\tchar time" (second ct) "in milliseconds")
        (println "\tstring time" (second st) "in milliseconds")
        (println "\tregex time" (second rt) "in milliseconds")
        (println "===")))

