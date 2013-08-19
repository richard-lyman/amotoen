;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.test.json
  (:import (java.io File))
  (:use [com.lithinos.amotoen.core] :reload-all)
  (:use [clojure.test])
  (:use [clojure.pprint])
  (:use [com.lithinos.amotoen.grammars
           [json :rename {grammar json-grammar}]]))

(deftest json-empty-object
    (let [r (to-ast :JSONText json-grammar (wrap-string "{}"))]
        (is (not (nil? r)))))

(deftest json-containing-object
    (let [r (to-ast :JSONText json-grammar (wrap-string "{\"a\":true,\"b\":false}"))]
        (is (not (nil? r)))))

(deftest json-empty-array
    (let [r (to-ast :JSONText json-grammar (wrap-string "[]"))]
        (is (not (nil? r)))))

(deftest json-containing-array
    (let [r (to-ast :JSONText json-grammar (wrap-string "[1,2,3]"))]
        (is (not (nil? r)))))

(deftest json-samples-in-out
    (doseq [f (filter #(re-find #"\.in$" (.getName %)) (file-seq (File. "./test/JSONTests")))]
        ;(spit "json.txt" (str f "\n\n") :append true)
        (let [c (.trim (slurp (File. (.replaceAll (.getCanonicalPath f) ".in$" ".out"))))
              r (to-ast :JSONText json-grammar (wrap-string (slurp f)))]
            ;(spit "json.txt" (pr-str r) :append true)
            ;(is (= c (pr-str r)))
            )))

