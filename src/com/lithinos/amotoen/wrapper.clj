;   Copyright (c) Richard Lyman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.lithinos.amotoen.wrapper)

(defprotocol IWrapper
    (consume?           [t terminal]    "Returns true if the given terminal can be successfully consumed.")
    (consume            [t terminal]    "Returns the result of consuming the given terminal.")
    (context            [t]             "Returns anything to describe the current context.")
    (cyclical?          [t terminal]    "Returns true if processing the given terminal would start an endless cycle because the terminal had been 'tracked' before.")
    (cyclical-pop       [t]             "Backs out of tracking a terminal.")
    (cyclical-track     [t terminal]    "Tracks the process of walking a grammar.")
    (debug              [t indent s]    )
    (debug-with-context [t indent s]    )
    (fail               [t throwable]   )
    (get-mark           [t]             "Returns anything that could be the parameter to and would result in a successful invocation of 'return-to-mark'")
    (end?               [t]             "Returns true if there is no more input")
    (reset              [t]             )
    (return-to-mark     [t mark]        "Used for backtracking")
    (set-debug          [t state]       ))

