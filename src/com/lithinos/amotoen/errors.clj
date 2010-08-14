;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.errors)

(def #^{:private true} *amotoen-throwable*            (Throwable. "Problem specific to using amotoen"))
(def #^{:private true} *amotoen-cyclical-throwable*   (Throwable. "Cyclical problem specific to using amotoen"))

(defn amotoen-error             [s] (Error. s *amotoen-throwable*))
(defn amotoen-cyclical-error    [s] (Error. s *amotoen-cyclical-throwable*))

(defn is-amotoen-error          [^Throwable e] (= (.getCause e) *amotoen-throwable*))
(defn is-amotoen-cyclical-error [^Throwable e] (= (.getCause e) *amotoen-cyclical-throwable*))

