;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.test.markdown
  (:import (java.io File))
  (:use [com.lithinos.amotoen.core] :reload-all)
  (:use [clojure.test])
  (:use [clojure.pprint])
  (:use [com.lithinos.amotoen.grammars.markdown]))

#_(deftest standard-test-files
    (doseq [f (list (first (filter #(re-find #"\.text$" (.getName %)) (file-seq (File. "./test/MarkdownTests")))))]
        (println f)
        (let [correct   (File. (.replaceAll (.getCanonicalPath f) ".text$" ".html"))
              result    (pegasus :Start grammar (wrap-string (slurp f))) ]
            (pprint result)
            ;(is (= result (slurp correct)))
            ))
            )

#_(deftest single-regularchar
    (let [mdp       (create-parser grammar)
          result    (mdp (wrap-string "a"))]))

#_(deftest newlines-and-blanklines
    (let [mdp       (create-parser grammar)]
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

