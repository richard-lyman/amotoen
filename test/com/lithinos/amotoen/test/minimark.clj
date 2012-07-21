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

