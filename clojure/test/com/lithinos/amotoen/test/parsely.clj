;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.test.parsely
  (:use [com.lithinos.amotoen.core] :reload-all)
  (:use [clojure.test])
  (:use [clojure.pprint]))

; :expr #{"x" ["(" :expr* ")"]}
(def g1 {:expr '(| \x [\( (* :expr) \)])})

(deftest g11 (is (= (to-ast :expr g1 (wrap-string "x"))        '{:expr \x})))
(deftest g12 (is (= (to-ast :expr g1 (wrap-string "()"))       '{:expr [\( () \)]})))
(deftest g13 (is (= (to-ast :expr g1 (wrap-string "(xx)"))     '{:expr [\( ({:expr \x} {:expr \x}) \)]})))
(deftest g14 (is (= (to-ast :expr g1 (wrap-string "((x)())"))  '{:expr [\( ({:expr [\( {:expr \x} \)]} {:expr [\( () \)]}) \)]})))

; Different parse trees
; :expr #{"x" :expr-rep}
; :expr-rep ["(" :expr* ")"]
(def g2 {:expr '(| \x [\( (* :expr) \)])})
(def g3 {:expr      '(| \x :expr-rep)
         :expr-rep  [\( '(* :expr) \)]})
(deftest g2_vs_g3   (let [r2 (to-ast :expr g2 (wrap-string "()"))
                          r3 (to-ast :expr g3 (wrap-string "()"))]
                        (is (= r2 {:expr [\( () \)]}))
                        (is (= r3 {:expr {:expr-rep [\( () \)]}}))
                        (is (not (= r2 r3)))))

; Force g2 and g3 to result in the same AST
                            ; Admittedly, this is uglier than I wish it were
(def g3prime    {:expr      (list '| \x (list 'f #(first (vals %)) :expr-rep))
                 :expr-rep  [\( '(* :expr) \)]})
(deftest g2_vs_g3prime  (let [r2 (to-ast :expr g2 (wrap-string "()"))
                              r3 (to-ast :expr g3prime (wrap-string "()"))]
                            (is (= r2 {:expr [\( () \)]}))
                            (is (= r3 {:expr [\( () \)]}))
                            (is (= r2 r3))))

(def p #(to-ast :expr g1 (wrap-string %)))

(deftest conform (is (not (nil? (p "(x(x))")))))
(deftest malformed (is (nil? (p "a(zldxn(dez)"))))

#_(doseq [g [ {:expr '(| \x \newline [\( (* :expr) \)])}
            {:expr '(| [\( (* :expr) \)] \x \newline)}
            {:expr (list '| #"^((x))" [\( '(* :expr) \)] \newline)}]]
    (println)
    (pprint g)
    (def p #(to-ast :expr g (wrap-string %)))
    (let [line (apply str "\n" (repeat 10 "((x))"))
          input (str "(" (apply str (repeat 1000 line)) ")")]
        (println "Size of input:" (count input))
        ;(time (fastest-theoretical-by-single input))
        (time (p input))
        (time (p (str "(" input ")")))))

