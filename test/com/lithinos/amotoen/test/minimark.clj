;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.test.minimark
  (:import (java.io File))
  (:use [com.lithinos.amotoen.core] :reload-all)
  (:use [clojure.test])
  (:use [clojure.pprint])
  (:use [com.lithinos.amotoen.grammars.minimark]))

(deftest single-safe-char       (is (= \a       (to-html "a"))))
(deftest single-escaped-char    (is (= \A       (to-html "!A"))))
(deftest multiple-safe-char     (is (= "aaa"    (to-html "aaa"))))
(deftest multiple-escaped-char  (is (= "AAA"    (to-html "!A!A!A"))))
(deftest mixed-char             (is (= "aA"     (to-html "a!A"))))
(deftest multiple-mixed-char    (is (= "aAaAaA" (to-html "a!Aa!Aa!A"))))

(deftest single-safe-newline    (is (= \newline                     (to-html "\n"))))
(deftest single-safe-empty-line (is (= "<div class='empty-line' />" (to-html "\n\n"))))
(deftest single-safe-space      (is (= \space                       (to-html " "))))
(deftest single-safe-tab        (is (= \tab                         (to-html "\t"))))

(deftest mix-safe-chars-and-whitepsace1 (is (= " a\tAa aaa<div class='empty-line' />\t\taA\n" (to-html " a\t!Aa aaa\n\n\t\ta!A\n"))))

(deftest hrule          (is (= "<hr />"                                         (to-html "----"))))
(deftest mdash          (is (= "&mdash;"                                        (to-html "---"))))
(deftest superscript    (is (= "<span class='superscript'>SuperScript</span>"   (to-html "^SuperScript^"))))
(deftest h4             (is (= "<div class='H4'> H4 </div>"                     (to-html "==== H4 ===="))))
(deftest h3             (is (= "<div class='H3'> H3 </div>"                     (to-html "=== H3 ==="))))
(deftest h2             (is (= "<div class='H2'> H2 </div>"                     (to-html "== H2 =="))))
(deftest h1             (is (= "<div class='H1'> H1 </div>"                     (to-html "= H1 ="))))
(deftest bold           (is (= "<span class='bold'>Bold</span>"                 (to-html "'''Bold'''"))))
(deftest italics        (is (= "<span class='italic'>Italics</span>"            (to-html "''Italics''"))))
(deftest underlined     (is (= "<span class='underline'>underlined</span>"      (to-html "__underlined__"))))
(deftest inline-pre     (is (= "<div class='pre-inline'>Inline Pre</div>"       (to-html "{{{Inline Pre}}}"))))
(deftest block-pre      (is (= "<div class='pre-block'>Block\nPre</div>"        (to-html "{{{\nBlock\nPre\n}}}"))))
(deftest href           (is (= "<a href='href'>href</a>"                        (to-html "[[href]]"))))
(deftest href-explained (is (= "<a href='href'>explained</a>"                   (to-html "[href explained]"))))

(deftest list-numbered  (is (= "<ol><li>Item</li></ol>" (to-html "1. Item\n"))))
(deftest list-unordered (is (= "<ul><li>Item</li></ul>" (to-html "-- Item\n"))))

(deftest multi-list-numbered    (is (= "<ol><li>Item 1</li><li>Item 2</li></ol>" (to-html "1. Item 1\n1. Item 2\n"))))
(deftest multi-list-unordered   (is (= "<ul><li>Item 1</li><li>Item 2</li></ul>" (to-html "-- Item 1\n-- Item 2\n"))))

(deftest list-numbered-with-markup
    (is (=  "<ol><li>Item <span class='superscript'>ss</span> <span class='underline'>u</span> <span class='bold'>b</span> <span class='italic'>i</span> <a href='href'>href</a> <div class='pre-inline'>pre</div> 1</li></ol>"
            (to-html "1. Item ^ss^ __u__ '''b''' ''i'' [[href]] {{{pre}}} 1\n"))))
(deftest list-unordered-with-markup
    (is (=  "<ul><li>Item <span class='superscript'>ss</span> <span class='underline'>u</span> <span class='bold'>b</span> <span class='italic'>i</span> <a href='href'>href</a> <div class='pre-inline'>pre</div> 1</li></ul>"
            (to-html "-- Item ^ss^ __u__ '''b''' ''i'' [[href]] {{{pre}}} 1\n"))))

(deftest multi-list-numbered-with-markup
    (is (=  "<ol><li>Item <span class='superscript'>ss</span> <span class='underline'>u</span> <span class='bold'>b</span> <span class='italic'>i</span> <a href='href'>href</a> <div class='pre-inline'>pre</div> 1</li><li>Item <span class='superscript'>ss</span> <span class='underline'>u</span> <span class='bold'>b</span> <span class='italic'>i</span> <a href='href'>href</a> <div class='pre-inline'>pre</div> 1</li></ol>"
            (to-html "1. Item ^ss^ __u__ '''b''' ''i'' [[href]] {{{pre}}} 1\n1. Item ^ss^ __u__ '''b''' ''i'' [[href]] {{{pre}}} 1\n"))))
(deftest multi-list-unordered-with-markup
    (is (=  "<ul><li>Item <span class='superscript'>ss</span> <span class='underline'>u</span> <span class='bold'>b</span> <span class='italic'>i</span> <a href='href'>href</a> <div class='pre-inline'>pre</div> 1</li><li>Item <span class='superscript'>ss</span> <span class='underline'>u</span> <span class='bold'>b</span> <span class='italic'>i</span> <a href='href'>href</a> <div class='pre-inline'>pre</div> 1</li></ul>"
            (to-html "-- Item ^ss^ __u__ '''b''' ''i'' [[href]] {{{pre}}} 1\n-- Item ^ss^ __u__ '''b''' ''i'' [[href]] {{{pre}}} 1\n"))))

