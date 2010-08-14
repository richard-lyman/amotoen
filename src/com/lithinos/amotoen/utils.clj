;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.utils)

(defn pp-grammar [grammar]
    (let [ks    (keys grammar)
          m     (+ 1 (apply max (map #(count (name %)) ks)))]
        (doseq [k (sort ks)]
            (printf (str "\t%" m "." m "s %s\n") k (k grammar)))))

(defn terminal?         [o] (@#'com.lithinos.amotoen.core/terminal?         o))
(defn strict-terminal?  [o] (@#'com.lithinos.amotoen.core/strict-terminal?  o))

(defn non-terminals [base grammar] (filter (complement terminal?) (flatten (base grammar))))

(defn extract [base grammar]
    (loop [previous-count   0
           base-set         (non-terminals base grammar)]
        (if (= (count base-set) previous-count)
            (filter #(not (= nil (first (vals %)))) (map #(let [k % v (% grammar)] {k v}) base-set))
            (recur  (count base-set)
                    (distinct (concat base-set (flatten (map #(non-terminals % grammar) base-set))))))))

(defn add [non-terminal grammar old-rules]
    (merge old-rules (apply merge (extract non-terminal grammar))))

(declare lookahead)

(defn first-terminal [non-terminal grammar]
    (if (strict-terminal? non-terminal)
        non-terminal
        (let [base (if (keyword? non-terminal)
                        (non-terminal grammar)
                        non-terminal)]
            (cond
                (strict-terminal? base) base
                (keyword? base)         (first-terminal base            grammar)
                (vector? base)          (first-terminal (first base)    grammar)
                (list? base)            (if (= '| (first base))
                                            nil;(lookahead      non-terminal    grammar)
                                            (first-terminal (second base)   grammar))
                true                    nil))))

(defn lookahead [non-terminal grammar]
    (let [base (non-terminal grammar)]
        (if (not= (first base) '|) (throw (Error. "Lookahead is only useful for option groups")))
        (map (fn [i] {(first-terminal i grammar) i}) (rest base))))

