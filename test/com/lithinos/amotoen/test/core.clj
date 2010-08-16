(ns com.lithinos.amotoen.test.core
  (:use [com.lithinos.amotoen.core] :reload-all)
  (:use [clojure.test])
  (:use [clojure.pprint])
  (:import (java.io File))
  (:use [com.lithinos.amotoen.string-wrapper]
        [com.lithinos.amotoen [markdown :rename {grammar markdown-grammar}]]))

(deftest standard-test-files
    (let [mdp (create-parser markdown-grammar :debug)]
        (pprint (mdp (wrap-string "

")))    
#_        (pprint (mdp (wrap-string "a")))
#_        (pprint (mdp (wrap-string "a
")))
#_        (pprint (mdp (wrap-string "a

")))
#_        (pprint (mdp (wrap-string "a

a")))
#_        (doseq [f (list (first (filter #(re-find #"\.text$" (.getName %)) (file-seq (File. "./test/MarkdownTests")))))]
            (println f)
            (let [correct (File. (.replaceAll (.getCanonicalPath f) ".text$" ".html"))]
                (is (= (mdp (wrap-string (slurp f))) (slurp correct)))))
                ))
