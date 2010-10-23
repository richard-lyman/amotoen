(ns com.lithinos.amotoen.test.core
  (:import (java.io File))
  (:use [com.lithinos.amotoen.core] :reload-all)
  (:use [clojure.test])
  (:use [clojure.pprint])
  (:use [com.lithinos.amotoen.string-wrapper]
        [com.lithinos.amotoen [markdown :rename {grammar markdown-grammar}]]))

(deftest standard-test-files
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
