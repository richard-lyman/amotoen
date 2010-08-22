(ns com.lithinos.amotoen.test.core
  (:use [com.lithinos.amotoen.core] :reload-all)
  (:use [clojure.test])
  (:use [clojure.pprint])
  (:import (java.io File))
  (:use [com.lithinos.amotoen.string-wrapper]
        [com.lithinos.amotoen [markdown :rename {grammar markdown-grammar}]]))

#_(deftest standard-test-files
    (let [mdp (create-parser markdown-grammar)]
        (doseq [f (list (first (filter #(re-find #"\.text$" (.getName %)) (file-seq (File. "./test/MarkdownTests")))))]
            (println f)
            (let [correct (File. (.replaceAll (.getCanonicalPath f) ".text$" ".html"))]
                (is (= (mdp (wrap-string (slurp f))) (slurp correct)))))
                ))

(deftest double-newlines
    (let [mdp       (create-parser markdown-grammar)
          result    (mdp (wrap-string "

"))]
        (pprint result)))

(deftest single-regularchar
    (let [mdp       (create-parser markdown-grammar)
          result    (mdp (wrap-string "a"))]))

(deftest regularchar-newline
    (let [mdp       (create-parser markdown-grammar)
          result    (mdp (wrap-string "a
"))]))

(deftest regularchar-double-newline
    (let [mdp       (create-parser markdown-grammar)
          result    (mdp (wrap-string "a

"))]
        (pprint result)))

(deftest regularchar-double-newline-regularchar
    (let [mdp       (create-parser markdown-grammar)
          result    (mdp (wrap-string "a

a"))]
        (pprint result)))

(deftest regularchar-double-newline-with-whitespace-regularchar
    (let [mdp       (create-parser markdown-grammar)
          result    (mdp (wrap-string "a
                
a"))]
        (pprint result)))
