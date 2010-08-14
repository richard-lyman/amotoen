;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.string-wrapper
    (:use (com.lithinos.amotoen errors wrapper))
    (:import (java.util.regex Pattern)))

(defn- until-first-newline [s]
    (let [result (apply
                    str
                    (seq
                        (take-while
                            (fn [c]
                                (not= c \newline))
                            s)))]
        (str result
            (if (not= (count s) (count result))
                "..."
                ""))))

(defn- number-of-newlines [s] (count (filter #(= % \newline) s)))

(defn- tabs [l]
    (loop [result ""
           count l]
        (if (= count 0) 
            result
            (recur  (str result " ")
                    (- count 1)))))

(defn wrap-string [#^String i]
    (let [limit         (count i)
          location      (ref 0)
          ;line          (ref 0)
          cycles        (ref [])
          debug-switch   (ref false)]

        (reify IWrapper

            (consume? [t terminal]
                (if (= @location limit) false)
                (if (instance? java.util.regex.Pattern terminal)
                    (not= (re-find terminal (subs i @location)) nil)
                    (.regionMatches i @location terminal 0 (count terminal))))

            (consume [t terminal]
                (try
                    (let [input-remainder   (subs i @location)
                          consumed          (if (instance? Pattern terminal)
                                                (re-find terminal input-remainder)
                                                terminal)
                          before            @location]
                        (dosync (alter location + (count consumed)))
                        ;(dosync (alter line + (number-of-newlines consumed)))
                        (if (> @location limit)
                            (throw (amotoen-error "Consumed more than available")))
                        (if @debug-switch (println (str "Consumed: '" consumed "' from '" terminal "' [" before "," @location "]")))
                        consumed)
                    (catch StringIndexOutOfBoundsException e (throw (amotoen-error "Consumed more than available")))))

            (context [t]
                (let [input-remainder   (subs i (max 0 @location))
                      remainder         (until-first-newline input-remainder)]
                    (subs   remainder
                            0
                            (min (count remainder) 40))))

            (cyclical? [t terminal]
                (let [sl    (filter #(= (first (keys %)) terminal) @cycles) ; Is there a way to speed this up? Specifically the anon fn
                      slv   (map #(first (vals %)) sl)  ; Maybe here too...
                      m     (get-mark t)]
                    (some #(= % m) slv)))   ; Maybe here too...

            (cyclical-pop       [t]             (dosync (alter cycles pop)))
            (cyclical-track     [t terminal]    (dosync (alter cycles conj {terminal (get-mark t)})))
            (debug              [t indent s]    (if @debug-switch (println (tabs indent) s)))
            (debug-with-context [t indent s]    (if @debug-switch (println (tabs indent) s (str "\t->'" (context t) "'<-"))))

            (fail [t throwable] (println "ERROR:")
                                (println "  Context: " (context t))
                                (println "           " \u25B2)
                                ;(println "  Line:" @line)
                                (println "  Character:" @location)
                                ;(print "  Parse path:")
                                (print "  Error Message:")
                                (println (.getMessage ^Throwable throwable)))

            (get-mark   [t] [@location @cycles])
            (end?       [t] (>= @location limit))
            (reset      [t]
                (dosync (ref-set location 0))
                ;(dosync (ref-set line 0))
                (dosync (ref-set cycles []))
                (dosync (ref-set debug-switch false)))
            (return-to-mark [t mark]    (dosync (ref-set location (first mark)) (ref-set cycles (second mark))) true)
            (set-debug      [t state]   (dosync (ref-set debug-switch state))))))

