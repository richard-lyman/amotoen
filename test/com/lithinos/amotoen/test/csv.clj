;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.test.csv
  (:import (java.io File))
  (:use [com.lithinos.amotoen.core] :reload-all)
  (:use [clojure.test])
  (:use [clojure.pprint])
  (:use [com.lithinos.amotoen.grammars.csv]))

(deftest one-drawback-of-vanilla-chars (is (= (to-clj " a   , b   , c   ") [["a   " "b   " "c   "]])))

(deftest plain              (is (not (nil? (pegasus :Document grammar (wrap-string "a,b,c"))))))
(deftest plain-multi-char   (is (not (nil? (pegasus :Document grammar (wrap-string "aaa,bbb,ccc"))))))
(deftest plain-multi-line   (is (not (nil? (pegasus :Document grammar (wrap-string "a,b,c
                                                                                    x,y,z"))))))
(deftest plain-multi-char-and-line  (is (not (nil? (pegasus :Document grammar (wrap-string "aaa,bbb,ccc
                                                                                            xxx,yyy,zzz"))))))
(deftest plain-multi-char-and-line+ (is (not (nil? (pegasus :Document grammar (wrap-string "aaa,bbb,ccc
                                                                                            iii,jjj,kkk
                                                                                            xxx,yyy,zzz"))))))
(deftest single                 (is (not (nil? (pegasus :Document grammar               (wrap-string "'a','b','c'"))))))
(deftest double-quote           (is (not (nil? (pegasus :Document grammar               (wrap-string "\"a\",\"b\",\"c\""))))))
(deftest specified-comma-square (is (not (nil? (pegasus :Document (specified \, \[ \])  (wrap-string "[a],[b],[c]"))))))
(deftest specified-pipe-square  (is (not (nil? (pegasus :Document (specified \| \[ \])  (wrap-string "[a]|[b]|[c]"))))))
(deftest specified-pipe-plus    (is (not (nil? (pegasus :Document (specified \| \+ \+)  (wrap-string "+a+|+b+|+c+"))))))

(deftest to-clj-plain (is (= (to-clj "a,b,c") [["a" "b" "c"]])))

(deftest to-clj-plain-multi-char (is (= (to-clj "aaa,bbb,ccc") [["aaa" "bbb" "ccc"]])))

(deftest to-clj-plain-multi-line (is (= (to-clj "   a,b,c
                                                    x,y,z") [["a" "b" "c"] ["x" "y" "z"]])))

(deftest to-clj-plain-multi-char-and-line   (is (= (to-clj "aaa,bbb,ccc
                                                            xxx,yyy,zzz") [["aaa" "bbb" "ccc"] ["xxx" "yyy" "zzz"]])))

(deftest to-clj-plain-multi-char-and-line+  (is (= (to-clj "aaa,bbb,ccc
                                                            iii,jjj,kkk
                                                            xxx,yyy,zzz") [["aaa" "bbb" "ccc"] ["iii" "jjj" "kkk"] ["xxx" "yyy" "zzz"]])))

(deftest to-clj-single                  (is (= (to-clj "'a','b','c'")                       [["a" "b" "c"]])))
(deftest to-clj-double                  (is (= (to-clj "\"a\",\"b\",\"c\"")                 [["a" "b" "c"]])))
(deftest to-clj-specified-comma-square  (is (= (to-clj "[a],[b],[c]" (specified \, \[ \]))  [["a" "b" "c"]])))
(deftest to-clj-specified-pipe-square   (is (= (to-clj "[a]|[b]|[c]" (specified \| \[ \]))  [["a" "b" "c"]])))
(deftest to-clj-specified-pipe-plus     (is (= (to-clj "+a+|+b+|+c+" (specified \| \+ \+))  [["a" "b" "c"]])))

(deftest to-clj-multiple-single                  (is (= (to-clj "'aaa','bbb','ccc'")                       [["aaa" "bbb" "ccc"]])))
(deftest to-clj-multiple-double                  (is (= (to-clj "\"aaa\",\"bbb\",\"ccc\"")                 [["aaa" "bbb" "ccc"]])))
(deftest to-clj-multiple-specified-comma-square  (is (= (to-clj "[aaa],[bbb],[ccc]" (specified \, \[ \]))  [["aaa" "bbb" "ccc"]])))
(deftest to-clj-multiple-specified-pipe-square   (is (= (to-clj "[aaa]|[bbb]|[ccc]" (specified \| \[ \]))  [["aaa" "bbb" "ccc"]])))
(deftest to-clj-multiple-specified-pipe-plus     (is (= (to-clj "+aaa+|+bbb+|+ccc+" (specified \| \+ \+))  [["aaa" "bbb" "ccc"]])))

(deftest to-clj-multi-char-and-line-single                  (is (= (to-clj "'aaa','bbb','ccc'
                                                                            'xxx','yyy','zzz'")                     [["aaa" "bbb" "ccc"] ["xxx" "yyy" "zzz"]])))
(deftest to-clj-multi-char-and-line-double                  (is (= (to-clj "\"aaa\",\"bbb\",\"ccc\"
                                                                            \"xxx\",\"yyy\",\"zzz\"")               [["aaa" "bbb" "ccc"] ["xxx" "yyy" "zzz"]])))
(deftest to-clj-multi-char-and-line-specified-comma-square  (is (= (to-clj "[aaa],[bbb],[ccc]
                                                                            [xxx],[yyy],[zzz]" (specified \, \[ \]))  [["aaa" "bbb" "ccc"] ["xxx" "yyy" "zzz"]])))
(deftest to-clj-multi-char-and-line-specified-pipe-square   (is (= (to-clj "[aaa]|[bbb]|[ccc]
                                                                            [xxx]|[yyy]|[zzz]" (specified \| \[ \]))  [["aaa" "bbb" "ccc"] ["xxx" "yyy" "zzz"]])))
(deftest to-clj-multi-char-and-line-specified-pipe-plus     (is (= (to-clj "+aaa+|+bbb+|+ccc+
                                                                            +xxx+|+yyy+|+zzz+" (specified \| \+ \+))  [["aaa" "bbb" "ccc"] ["xxx" "yyy" "zzz"]])))

(deftest to-clj-multi-char-and-line-single+                 (is (= (to-clj "'aaa','bbb','ccc'
                                                                            'iii','jjj','kkk'
                                                                            'xxx','yyy','zzz'")                     [["aaa" "bbb" "ccc"] ["iii" "jjj" "kkk"] ["xxx" "yyy" "zzz"]])))
(deftest to-clj-multi-char-and-line-double+                 (is (= (to-clj "\"aaa\",\"bbb\",\"ccc\"
                                                                            \"iii\",\"jjj\",\"kkk\"
                                                                            \"xxx\",\"yyy\",\"zzz\"")               [["aaa" "bbb" "ccc"] ["iii" "jjj" "kkk"] ["xxx" "yyy" "zzz"]])))
(deftest to-clj-multi-char-and-line-specified-comma-square+ (is (= (to-clj "[aaa],[bbb],[ccc]
                                                                            [iii],[jjj],[kkk]
                                                                            [xxx],[yyy],[zzz]" (specified \, \[ \]))  [["aaa" "bbb" "ccc"] ["iii" "jjj" "kkk"] ["xxx" "yyy" "zzz"]])))
(deftest to-clj-multi-char-and-line-specified-pipe-square+  (is (= (to-clj "[aaa]|[bbb]|[ccc]
                                                                            [iii]|[jjj]|[kkk]
                                                                            [xxx]|[yyy]|[zzz]" (specified \| \[ \]))  [["aaa" "bbb" "ccc"] ["iii" "jjj" "kkk"] ["xxx" "yyy" "zzz"]])))
(deftest to-clj-multi-char-and-line-specified-pipe-plus+    (is (= (to-clj "+aaa+|+bbb+|+ccc+
                                                                            +iii+|+jjj+|+kkk+
                                                                            +xxx+|+yyy+|+zzz+" (specified \| \+ \+))  [["aaa" "bbb" "ccc"] ["iii" "jjj" "kkk"] ["xxx" "yyy" "zzz"]])))


(deftest with-vanilla-single                    (is (not (nil? (pegasus :Document grammar               (wrap-string "'a',b,'c'"))))))
(deftest with-vanilla-double-quote              (is (not (nil? (pegasus :Document grammar               (wrap-string "\"a\",b,\"c\""))))))
(deftest with-vanilla-specified-comma-square    (is (not (nil? (pegasus :Document (specified \, \[ \])  (wrap-string "[a],b,[c]"))))))
(deftest with-vanilla-specified-pipe-square     (is (not (nil? (pegasus :Document (specified \| \[ \])  (wrap-string "[a]|b|[c]"))))))
(deftest with-vanilla-specified-pipe-plus       (is (not (nil? (pegasus :Document (specified \| \+ \+)  (wrap-string "+a+|b|+c+"))))))

(deftest with-vanilla-to-clj-single                  (is (= (to-clj "'a',b,'c'")                        [["a" "b" "c"]])))
(deftest with-vanilla-to-clj-double                  (is (= (to-clj "\"a\",b,\"c\"")                    [["a" "b" "c"]])))
(deftest with-vanilla-to-clj-specified-comma-square  (is (= (to-clj "[a],b,[c]" (specified \, \[ \]))   [["a" "b" "c"]])))
(deftest with-vanilla-to-clj-specified-pipe-square   (is (= (to-clj "[a]|b|[c]" (specified \| \[ \]))   [["a" "b" "c"]])))
(deftest with-vanilla-to-clj-specified-pipe-plus     (is (= (to-clj "+a+|b|+c+" (specified \| \+ \+))   [["a" "b" "c"]])))

(deftest with-vanilla-to-clj-multiple-single                  (is (= (to-clj "'aaa',bbb,'ccc'")                     [["aaa" "bbb" "ccc"]])))
(deftest with-vanilla-to-clj-multiple-double                  (is (= (to-clj "\"aaa\",bbb,\"ccc\"")                 [["aaa" "bbb" "ccc"]])))
(deftest with-vanilla-to-clj-multiple-specified-comma-square  (is (= (to-clj "[aaa],bbb,[ccc]" (specified \, \[ \]))[["aaa" "bbb" "ccc"]])))
(deftest with-vanilla-to-clj-multiple-specified-pipe-square   (is (= (to-clj "[aaa]|bbb|[ccc]" (specified \| \[ \]))[["aaa" "bbb" "ccc"]])))
(deftest with-vanilla-to-clj-multiple-specified-pipe-plus     (is (= (to-clj "+aaa+|bbb|+ccc+" (specified \| \+ \+))[["aaa" "bbb" "ccc"]])))

(deftest with-vanilla-to-clj-multi-char-and-line-single                  (is (= (to-clj "'aaa',bbb,'ccc'
                                                                                         'xxx',yyy,'zzz'")                      [["aaa" "bbb" "ccc"] ["xxx" "yyy" "zzz"]])))
(deftest with-vanilla-to-clj-multi-char-and-line-double                  (is (= (to-clj "\"aaa\",bbb,\"ccc\"
                                                                                         \"xxx\",yyy,\"zzz\"")                  [["aaa" "bbb" "ccc"] ["xxx" "yyy" "zzz"]])))
(deftest with-vanilla-to-clj-multi-char-and-line-specified-comma-square  (is (= (to-clj "[aaa],bbb,[ccc]
                                                                                         [xxx],yyy,[zzz]" (specified \, \[ \])) [["aaa" "bbb" "ccc"] ["xxx" "yyy" "zzz"]])))
(deftest with-vanilla-to-clj-multi-char-and-line-specified-pipe-square   (is (= (to-clj "[aaa]|bbb|[ccc]
                                                                                         [xxx]|yyy|[zzz]" (specified \| \[ \])) [["aaa" "bbb" "ccc"] ["xxx" "yyy" "zzz"]])))
(deftest with-vanilla-to-clj-multi-char-and-line-specified-pipe-plus     (is (= (to-clj "+aaa+|bbb|+ccc+
                                                                                         +xxx+|yyy|+zzz+" (specified \| \+ \+)) [["aaa" "bbb" "ccc"] ["xxx" "yyy" "zzz"]])))

(deftest with-vanilla-to-clj-multi-char-and-line-single+                 (is (= (to-clj "'aaa',bbb,'ccc'
                                                                                         'iii',jjj,'kkk'
                                                                                         'xxx',yyy,'zzz'")                      [["aaa" "bbb" "ccc"] ["iii" "jjj" "kkk"] ["xxx" "yyy" "zzz"]])))
(deftest with-vanilla-to-clj-multi-char-and-line-double+                 (is (= (to-clj "\"aaa\",bbb,\"ccc\"
                                                                                         \"iii\",jjj,\"kkk\"
                                                                                         \"xxx\",yyy,\"zzz\"")                  [["aaa" "bbb" "ccc"] ["iii" "jjj" "kkk"] ["xxx" "yyy" "zzz"]])))
(deftest with-vanilla-to-clj-multi-char-and-line-specified-comma-square+ (is (= (to-clj "[aaa],bbb,[ccc]
                                                                                         [iii],jjj,[kkk]
                                                                                         [xxx],yyy,[zzz]" (specified \, \[ \])) [["aaa" "bbb" "ccc"] ["iii" "jjj" "kkk"] ["xxx" "yyy" "zzz"]])))
(deftest with-vanilla-to-clj-multi-char-and-line-specified-pipe-square+  (is (= (to-clj "[aaa]|bbb|[ccc]
                                                                                         [iii]|jjj|[kkk]
                                                                                         [xxx]|yyy|[zzz]" (specified \| \[ \])) [["aaa" "bbb" "ccc"] ["iii" "jjj" "kkk"] ["xxx" "yyy" "zzz"]])))
(deftest with-vanilla-to-clj-multi-char-and-line-specified-pipe-plus+    (is (= (to-clj "+aaa+|bbb|+ccc+
                                                                                         +iii+|jjj|+kkk+
                                                                                         +xxx+|yyy|+zzz+" (specified \| \+ \+)) [["aaa" "bbb" "ccc"] ["iii" "jjj" "kkk"] ["xxx" "yyy" "zzz"]])))

(def nightmare-string "a,b,c
aaa,bbb,ccc
a,b,c
x,y,z
 aaa,bbb,ccc
 xxx,yyy,zzz
'a','b','c'
\"a\",\"b\",\"c\"
[a],[b],[c]
[a]|[b]|[c]
+a+|+b+|+c+
a,b,c
aaa,bbb,ccc
   a,b,c
   x,y,z
  aaa,bbb,ccc
  xxx,yyy,zzz
'a','b','c'
\"a\",\"b\",\"c\"
[a],[b],[c]
[a]|[b]|[c]
+a+|+b+|+c+
'aaa','bbb','ccc'
\"aaa\",\"bbb\",\"ccc\"
[aaa],[bbb],[ccc]
[aaa]|[bbb]|[ccc]
+aaa+|+bbb+|+ccc+
'aaa','bbb','ccc'
'xxx','yyy','zzz'
\"aaa\",\"bbb\",\"ccc\"
\"xxx\",\"yyy\",\"zzz\"
[aaa],[bbb],[ccc]
[xxx],[yyy],[zzz]
[aaa]|[bbb]|[ccc]
[xxx]|[yyy]|[zzz]
+aaa+|+bbb+|+ccc+
+xxx+|+yyy+|+zzz+
'a',b,'c'
\"a\",b,\"c\"
[a],b,[c]
[a]|b|[c]
+a+|b|+c+
'a',b,'c'
\"a\",b,\"c\"
[a],b,[c]
[a]|b|[c]
+a+|b|+c+
'aaa',bbb,'ccc'
\"aaa\",bbb,\"ccc\"
[aaa],bbb,[ccc]
[aaa]|bbb|[ccc]
+aaa+|bbb|+ccc+
'aaa',bbb,'ccc'
'xxx',yyy,'zzz'
\"aaa\",bbb,\"ccc\"
\"xxx\",yyy,\"zzz\"
[aaa],bbb,[ccc]
[xxx],yyy,[zzz]
[aaa]|bbb|[ccc]
[xxx]|yyy|[zzz]
+aaa+|bbb|+ccc+
+xxx+|yyy|+zzz+")
(def nightmare-string-with-whitespace "a,b,c
aaa,bbb,ccc
a,b,c
x,y,z
 aaa,bbb,ccc
 xxx,yyy,zzz
'a',            'b','c'
\"a\",\"b\",\"c\"
[a],[b],[c]
[a]|[b]|[c]
+a+|+b+|+c+
a,b,c
aaa,bbb,ccc
   a,b,c
   x,y,z
  aaa,bbb,ccc
  xxx,yyy,zzz
'a','b','c'
\"a\",\"b\",\"c\"
[a],[b],[c]                          
[a]|[b]|[c]
+a+|+b+|+c+
'aaa','bbb',           'ccc'
\"aaa\",\"bbb\",\"ccc\"
[aaa],[bbb],[ccc]
[aaa]|[bbb]|[ccc]
+aaa+|+bbb+|+ccc+
'aaa','bbb','ccc'
'xxx','yyy','zzz'
\"aaa\"              ,\"bbb\",\"ccc\"
\"xxx\",\"yyy\",\"zzz\"
                      [aaa],[bbb],[ccc]
[xxx],[yyy],[zzz]
[aaa]|[bbb]|[ccc]
[xxx]|[yyy]|[zzz]
+aaa+            |+bbb+               |+ccc+
+xxx+|+yyy+|+zzz+
'a',b,'c'
\"a\",b,\"c\"
[a],b,[c]
[a]|b|[c]
                       +a+|b|+c+
'a',b,'c'
\"a\",b,\"c\"
[a],b,[c]
[a]|b|[c]                          
+a+|b|+c+
'aaa',bbb,                    'ccc'
\"aaa\",bbb,             \"ccc\"
[aaa],bbb,[ccc]
[aaa]|bbb|[ccc]
+aaa+|bbb|+ccc+
'aaa',bbb,'ccc'
'xxx',yyy,'zzz'
                      \"aaa\",bbb,\"ccc\"                           
\"xxx\",yyy,\"zzz\"
[aaa],bbb,[ccc]
[xxx],yyy,[zzz]
[aaa]|bbb|[ccc]
[xxx]|yyy|[zzz]
+aaa+|bbb|+ccc+
+xxx+|yyy|+zzz+")
(def nightmare-parsed [ ["a" "b" "c"]
                        ["aaa" "bbb" "ccc"]
                        ["a" "b" "c"]
                        ["x" "y" "z"]
                        ["aaa" "bbb" "ccc"]
                        ["xxx" "yyy" "zzz"]
                        ["a" "b" "c"]
                        ["a" "b" "c"]
                        ["a" "b" "c"]
                        ["a" "b" "c"]
                        ["a" "b" "c"]
                        ["a" "b" "c"]
                        ["aaa" "bbb" "ccc"]
                        ["a" "b" "c"]
                        ["x" "y" "z"]
                        ["aaa" "bbb" "ccc"]
                        ["xxx" "yyy" "zzz"]
                        ["a" "b" "c"]
                        ["a" "b" "c"]
                        ["a" "b" "c"]
                        ["a" "b" "c"]
                        ["a" "b" "c"]
                        ["aaa" "bbb" "ccc"]
                        ["aaa" "bbb" "ccc"]
                        ["aaa" "bbb" "ccc"]
                        ["aaa" "bbb" "ccc"]
                        ["aaa" "bbb" "ccc"]
                        ["aaa" "bbb" "ccc"]
                        ["xxx" "yyy" "zzz"]
                        ["aaa" "bbb" "ccc"]
                        ["xxx" "yyy" "zzz"]
                        ["aaa" "bbb" "ccc"]
                        ["xxx" "yyy" "zzz"]
                        ["aaa" "bbb" "ccc"]
                        ["xxx" "yyy" "zzz"]
                        ["aaa" "bbb" "ccc"]
                        ["xxx" "yyy" "zzz"]
                        ["a" "b" "c"]
                        ["a" "b" "c"]
                        ["a" "b" "c"]
                        ["a" "b" "c"]
                        ["a" "b" "c"]
                        ["a" "b" "c"]
                        ["a" "b" "c"]
                        ["a" "b" "c"]
                        ["a" "b" "c"]
                        ["a" "b" "c"]
                        ["aaa" "bbb" "ccc"]
                        ["aaa" "bbb" "ccc"]
                        ["aaa" "bbb" "ccc"]
                        ["aaa" "bbb" "ccc"]
                        ["aaa" "bbb" "ccc"]
                        ["aaa" "bbb" "ccc"]
                        ["xxx" "yyy" "zzz"]
                        ["aaa" "bbb" "ccc"]
                        ["xxx" "yyy" "zzz"]
                        ["aaa" "bbb" "ccc"]
                        ["xxx" "yyy" "zzz"]
                        ["aaa" "bbb" "ccc"]
                        ["xxx" "yyy" "zzz"]
                        ["aaa" "bbb" "ccc"]
                        ["xxx" "yyy" "zzz"]])
(def nightmare-grammar (specified (lpegs '| ",|") (lpegs '| "'\"[+") (lpegs '| "'\"]+")))
(deftest nightmare-string-to-clj                    (is (= (to-clj nightmare-string                 nightmare-grammar) nightmare-parsed)))
(deftest nightmare-string-with-whitespace-to-clj    (is (= (to-clj nightmare-string-with-whitespace nightmare-grammar) nightmare-parsed)))
