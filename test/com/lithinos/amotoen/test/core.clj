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
           [csv :rename {grammar csv-grammar specified csv-specified}]
           [json :rename {grammar json-grammar}]
           [minimark :rename {grammar minimark-grammar}]
            ]))

(defn test-grammars []
    (let [cf #(apply str %)]
        (doseq [g [ {:S \a}
                    {:S (pegs "}}}")}
                    {:S (list '% (pegs "}}}"))}
                    {:S (list '* (list '% (pegs "}}}")))}
                    {:S [(list '* (list '% (pegs "}}}"))) (pegs "}}}")]}
                    {:S [(list 'f cf (pegs "abcabc"))]}
                    {:S [(list 'f cf (list '* (lpegs '| "abc")))]}
                    {:S [(list 'f cf '(* (| :A :B :C)))] :A \a :B \b :C \c }
                    csv-grammar
                    json-grammar
                    minimark-grammar]]
            (is (first (validate g))))))

(defn vectors-reset-pos []
    (let [g {:S [(list '* (list '% (pegs "}}}"))) (pegs "}}}")]}
          i "a}}b}}}"
          r (pegasus :S g (wrap-string i))]
        (is (= '{:S [(\a \} \} \b) [\} \} \}]]} r))))

(defn collapse-pegs []
    (let [custom-collapse #(apply str %)
          g {:S [(list 'f custom-collapse (pegs "abcabc"))]}
          i "abcabc"
          r (pegasus :S g (wrap-string i))]
        (is (= '{:S ["abcabc"]} r))))

(defn collapse-lpegs []
    (let [custom-collapse #(apply str %)
          g {:S [(list 'f custom-collapse (list '* (lpegs '| "abc")))]}
          i "aabbcc"
          r (pegasus :S g (wrap-string i))]
        (is (= '{:S ["aabbcc"]} r))))

(defn collapse-keywords []
    (let [custom-collapse (fn [r] (apply str (map #(first (vals %)) r)))
          g {:S [(list 'f custom-collapse '(* (| :A :B :C)))] :A \a :B \b :C \c }
          i "aabbcc"
          r (pegasus :S g (wrap-string i))]
        (is (= '{:S ["aabbcc"]} r))))

(do
    (println "Speed tests")
    (let [fastest   (with-out-str (time (self-check-fastest)))
          single    (with-out-str (time (self-check)))
          avg50     (with-out-str (time (doall (take 50 (repeatedly #(self-check))))))
          extract   #(Double/parseDouble (nth (.split % " ") 2)) ]
        (printf "%8.2f - Single Run\n" (extract single))
        (printf "%8.2f - Average over 50 runs\n" (/ (extract avg50) 50))
        (printf "%8.2f - Fastest theoretically possible\n" (extract fastest))
        (println "\n (in milliseconds)\n")))

(do
    (println "Corner cases")
    (test-grammars)
    (vectors-reset-pos)
    (collapse-lpegs)
    (collapse-keywords)
    (collapse-pegs))

(try
    (pegasus :S {:A :B} (wrap-string "fail"))
    (throw (Error. "A useful error should be thrown when a keyword doesn't exist in a grammar"))
    (catch Error e))

