(ns com.lithinos.amotoen.test.core
  (:import (java.io File))
  (:use [com.lithinos.amotoen.core] :reload-all)
  (:use [clojure.test])
  (:use [clojure.pprint])
  #_(:use [com.lithinos.amotoen.string-wrapper]
        [com.lithinos.amotoen [markdown :rename {grammar markdown-grammar}]])
        )

#_(deftest standard-test-files
    (let [mdp (create-parser markdown-grammar)]
        (doseq [f (list (first (filter #(re-find #"\.text$" (.getName %)) (file-seq (File. "./test/MarkdownTests")))))]
            (println f)
            (let [correct   (File. (.replaceAll (.getCanonicalPath f) ".text$" ".html"))
                  result    (markdown-to-html (mdp (wrap-string (slurp f))))]
                ;(is (= result (slurp correct)))
                ))
                ))

#_(deftest single-regularchar
    (let [mdp       (create-parser markdown-grammar)
          result    (mdp (wrap-string "a"))]))

#_(deftest newlines-and-blanklines
    (let [mdp       (create-parser markdown-grammar)]
        (pprint (mdp (wrap-string "

")))
        (pprint (mdp (wrap-string "a
")))
        (pprint (mdp (wrap-string "a

")))
        (pprint (mdp (wrap-string "a

a")))
        (pprint (mdp (wrap-string "a
                
a")))))

(defn test-grammars []
    (doseq [g [ {:S \a}
                {:S (pegs "}}}")}
                {:S (list '% (pegs "}}}"))}
                {:S (list '* (list '% (pegs "}}}")))}]]
        (when (not (first (validate g))) (throw (Error. (str "Invalid grammar: " (pr-str g)))))))

(defn vectors-reset-pos []
    (let [g {:S [(list '* (list '% (pegs "}}}"))) (pegs "}}}")]}
          i "a}}b}}}"
          r (pegasus :S g (gen-ps i))]
        (when (not= '{:S [(\a \} \} \b) [\} \} \}]]} r) (throw (Error. "Failed Vectors are not resetting the pos.")))
        true))

(defn collapse-pegs []
    (let [custom-collapse #(apply str %)
          g {:S [(list 'f custom-collapse (pegs "abcabc"))]}
          i "abcabc"
          r (pegasus :S g (gen-ps i))]
        (when (not= '{:S ["abcabc"]} r) (throw (Error. (str "pegs didn't collapse: " r))))
        true))

(defn collapse-lpegs []
    (let [custom-collapse #(apply str %)
          g {:S [(list 'f custom-collapse (list '* (lpegs '| "abc")))]}
          i "aabbcc"
          r (pegasus :S g (gen-ps i))]
        (when (not= '{:S ["aabbcc"]} r) (throw (Error. (str "lpegs didn't collapse: " r))))
        true))

(defn collapse-keywords []
    (let [custom-collapse (fn [r] (apply str (map #(first (vals %)) r)))
          g {:S [(list 'f custom-collapse '(* (| :A :B :C)))] :A \a :B \b :C \c }
          i "aabbcc"
          r (pegasus :S g (gen-ps i))]
        (when (not= '{:S ["aabbcc"]} r) (throw (Error. (str "keywords didn't collapse: " r))))
        true))

;(dosync (ref-set *debug* true))
(println "Single run") (time (self-check))
;(println "Single run" (self-check))
;(println "Dump" (time (self-ast)))
;(println "10 runs") (time (doall (take 10 (repeatedly #(self-check)))))
(println "40 runs") (time (doall (take 40 (repeatedly #(self-check)))))

;(println "start")
;(let [i (ref 0) j (ref 0)]
;    (println (first (keep #(do (dosync (alter i inc)) (if (even? %) :a nil)) [1 1 1 1 2 1 1 1 2])))
;    (println (loop []
;                (dosync (alter j inc))
;                (when (< (rand-int 100) 90)
;                    (recur))))
;    (println "end: " @i @j))


;(test-grammars)
;(vectors-reset-pos)
;(collapse-lpegs)
;(collapse-keywords)
;(collapse-pegs)

#_(try
    (pegasus :S {:A :B} (gen-ps "fail"))
    (throw (Error. "A useful error should be thrown when a keyword doesn't exist in a grammar"))
    (catch Error e))

