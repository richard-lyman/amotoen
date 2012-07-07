
    Copyright (c) Richard Lyman. All rights reserved.
    The use and distribution terms for this software are covered by the
    Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
    which can be found in the file epl-v10.html at the root of this distribution.
    By using this software in any fashion, you are agreeing to be bound by
    the terms of this license.
    You must not remove this notice, or any other, from this software.

<h2>Table of Contents</h2>

*   Name and Pronunciation
*   Introduction
*   Grammar Definitions

<h2>Name and Pronunciation</h2>

There is a PEG/Packrat library *Neotoma* (a scientific classification for pack 
rats) -- Amotoen is Neotoma written in reverse, not that there is any particular 
quality or relationship shared between the two; I was just taking the shortest path 
to a unique name that might have been tangentially related to PEGs. You might even 
successfully define 'shortest path' as some stream-of-consciousness process.

I tend to pronounce Amotoen with a style that follows: Am-o-toe-n.
I tend to place emphasis on the 'toe'.


<h2>Introduction</h2>

Amotoen is a Clojure library that supports PEG style definitions of grammars that can produce parsers.
While there are academic papers available that rigorously define PEG, I've found
that PEGs, or **P**arsing **E**xpression **G**rammar(s), are best explained by the 
[related Wikipedia page](http://en.wikipedia.org/wiki/Parsing_expression_grammar).

The clj-peg library was a predecessor to Amotoen and as such, Amotoen syntax might be reminiscent
of the syntax in clj-peg. There are, however, significant differences between 
using clj-peg and using Amotoen. The most significant of those differences can be 
found in the lack of macros, gen-class, or gen-interface. The clj-peg library used
*the dirty three* in the core, and Amotoen avoids all of them entirely.

Amotoen uses runtime processing of a given grammar, where clj-peg used macro 
time expansion of a grammar. Amotoen uses protocols instead of interfaces. Amotoen
internally uses nils instead of gen-classed Errors. All of these changes
result in far greater ease of use as well as increased maintainence.

In other words: **Amotoen is better than clj-peg. Amotoen is not AOT'd**.

<h2>Grammar Definitions</h2>

The grammar for Amotoen grammars is:

    {
        :Whitespace     '(| \space \newline \tab \,)
        :_*             '(* :Whitespace)
        :_              [:Whitespace '(* :Whitespace)]
        :Grammar        [\{ :_* :Rule '(* [:_ :Rule]) :_* \}]
        :Rule           [:Keyword :_ :Body]
        :Keyword        [\: :ValidKeywordChar '(* :ValidKeywordChar)]
        :Body           '(| :Keyword :Char :Grouping)
        :Grouping       '(| :Sequence :Either :ZeroOrMore :ZeroOrOne :AnyNot)
        :Sequence       [\[     :_* :Body '(* [:_* :Body])  :_* \]]
        :Either         [\( \|  :_  :Body '(* [:_* :Body])  :_* \)]
        :ZeroOrMore     [\( \*  :_  :Body                   :_* \)]
        :ZeroOrOne      [\( \?  :_  :Body                   :_* \)]
        :AnyNot         [\( \%  :_  '(| :Keyword :Char)     :_* \)]
        :Char           [\\ '(| :TabChar :SpaceChar :NewlineChar (% \space))]
        :TabChar        (pegs "tab")
        :SpaceChar      (pegs "space")
        :NewlineChar    (pegs "newline")
        :ValidKeywordChar (lpegs '| "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789:/*+!_?-")
    }

<h2>Recent Improvements</h2>

You can now supply a 'custom collapse' function to elements like '\*.
Since the switch to a more character-based process, there have been annoying structures that 
    represent little other than a set of characters that could better serve reduced to a string.

As an example:
 - Some function named 'custom-collapse' set to
    #(apply str %)
 - Some grammar 'g' set to
    {:S [(list custom-collapse (pegs "abcabc"))]}
 - Some input 'i' set to 
    "abcabc"
 - An invocation like...
    (pegasus :S g (gen-ps i))
 - ... should return...
    {:S ["abcabc"]}

Without supplying a custom collapse function:
 - Some grammar 'g' set to
    {:S (pegs "abcabc")}
 - Other things alike, the result should be
    {:S [\a \b \c \a \b \c]}

Another example:
 - Some function named 'custom-collapse' set to
    #(apply str %)
 - Some grammar 'g' set to
    {:S [(list custom-collapse '* (lpegs '| "abc"))]}
 - Some input 'i' set to 
    "aabbcc"
 - An invocation like...
    (pegasus :S g (gen-ps i))
 - ... should return...
    {:S ["aabbcc"]}

Without supplying a custom collapse function:
 - Some grammar 'g' set to
    {:S [(list '* (lpegs '| "abc"))]}
 - Other things alike, the result should be
    {:S [(\a \a \b \b \c \c)]}




