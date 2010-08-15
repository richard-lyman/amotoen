
    Copyright (c) Richard Lyman. All rights reserved.
    The use and distribution terms for this software are covered by the
    Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
    which can be found in the file epl-v10.html at the root of this distribution.
    By using this software in any fashion, you are agreeing to be bound by
    the terms of this license.
    You must not remove this notice, or any other, from this software.

*   [Name and Pronunciation](#name)
*   [Introduction](#intro)
*   [Samples](#samples)
    *   [JSON](#json)
    *   [Markdown](#markdown)
*   [Grammar Definitions](#grammar)

<h2 id="name">Name and Pronunciation</h2>

There is a PEG/Packrat library *Neotoma* (a scientific classification for pack 
rats) -- Amotoen is Neotoma written in reverse, not that there is any particular 
quality or relationship shared between the two; I was just taking the shortest path 
to a unique name that might have been tangentially related to PEGs. You might even 
successfully define 'shortest path' as some stream-of-consciousness process.

I tend to pronounce Amotoen with a style that follows: Am-o-toe-n.
I tend to place emphasis on the 'toe'.


<h2 id="intro">Introduction</h2>

Amotoen is a Clojure library that supports PEG style definitions of grammars that can produce parsers.
While there are academic papers available that rigorously define PEG, I've found
that PEGs, or **P**arsing **E**xpression **G**rammar(s), are best explained by the 
[related Wikipedia page](http://en.wikipedia.org/wiki/Parsing_expression_grammar).

The clj-peg library was a predecessor to Amotoen and as such, Amotoen keeps the 
feel of the syntax in clj-peg. There are, however, significant differences between 
using clj-peg and using Amotoen. The most significant of those differences can be 
found in the lack of macros, gen-class, or gen-interface. The clj-peg library used
*the dirty three* in the core, and Amotoen avoids all of them entirely.

Amotoen uses runtime processing of a given grammar, where clj-peg used macro 
time expansion of a grammar. Amotoen uses protocols instead of interfaces. Amotoen
uses internally defined Throwables instead of gen-classed Errors. All of these changes
result in far greater ease of use as well as increased maintainence.


<h2 id="samples">Samples</h2>

There are three simple commands to get started playing with Amotoen.

First, clone the git repo:

    git clone git://github.com/richard-lyman/amotoen.git

Second, change the current working directory to the newly created amotoen directory:

    cd amotoen

Third, start a Clojure repl with the Amotoen jar that we cloned:

    lein repl

<h3 id="json">JSON</h3>

In the REPL load some basic libraries:

    user=> (use '(com.lithinos.amotoen core string-wrapper) 
                '(com.lithinos.amotoen.grammars json))
    
Use the provided JSON grammar to create a JSON parser:

    user=> (def jsonp (create-parser grammar))

Throw some JSON at your parser (after wrapping it in the provided string-wrapper):

    user=> (pprint (jsonp (wrap-string "1")))

You'll see the structure resulting from that particular grammar's parser processing the wrapped input `"1"`:

    {:JSONRoot
     [{:_* ""}
      ({:Value {:JSONNumber {:Int {:Digit "1"}}}})
      {:_* ""}
      {:$ :EOF}]}

That resulting structure is a native Clojure data structure, nothing special about it.


<h3><a id="markdown">Markdown</a></h3>

In the REPL load some basic libraries:

    user=> (use '(com.lithinos.amotoen core string-wrapper)
                '(com.lithinos.amotoen [markdown :rename {grammar markdown-grammar}]))

Use the provided and renamed markdown grammar to create a markdown parser:

    user=> (def mdp (create-parser markdown-grammar))

Throw some markdown at your parser (after wrapping it in the provided string-wrapper):

    user=> (pprint (mdp (wrap-string "[1][]")))

You'll see the structure resulting from that particular grammar's parser processing the wrapped input `"[1][]"`:

    {:Document
     [({:Line
        ({:Span
          {:Link
           ["["
            {:LinkTextOrLabel "1"}
            "]"
            {:ReferenceLink {:ImplicitRefLink [() "[]"]}}]}})})
      {:$ :EOF}]}

That resulting structure is a native Clojure data structure, nothing special about it.


<h2 id="grammar">Grammar Definitions</h2>

The JSON grammar used above is shown [here](http://github.com/richard-lyman/amotoen/blob/master/src/com/lithinos/amotoen/grammars/json.clj#L11-48).

The markdown grammar used above is shown [here](http://github.com/richard-lyman/amotoen/blob/master/src/com/lithinos/amotoen/markdown.clj#L11-75).

I'll be adding more documentation on writing grammars soon...

