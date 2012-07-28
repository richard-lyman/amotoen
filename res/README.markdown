
    Copyright (c) Richard Lyman. All rights reserved.
    The use and distribution terms for this software are covered by the
    Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
    which can be found in the file epl-v10.html at the root of this distribution.
    By using this software in any fashion, you are agreeing to be bound by
    the terms of this license.
    You must not remove this notice, or any other, from this software.

<h2>Table of Contents</h2>

*   Basic Definitions
*   Name and Pronunciation
*   Introduction to Reading Grammars (or, Don't Be Afraid)
*   Introduction to Using Grammars (or, It's Also Easy)
*   Relation to clj-peg
*   Reading Grammars
*   Redefining Amotoen's Functionality
*   Amotoen Grammar Definition
*   Recent Improvements

<h2>Basic Definitions</h2>

There are several words used in this README that might be made clearer for the beginner through short simple definitions.
 - **Parser**: a program that takes some input, like a string or a file, and creates something else, like a structure describing the contents of the string or file
 - **PEG**: a way of defining a parser
 - **Grammar**: exactly the same as the concept of a grammar for a spoken language --- it provides structure to help in creating *valid* sentences
 - **Map**: a structure in programming that acts like a dictionary where you can find a definition by looking for the related word
 - **Vector**: a structure in programming that acts like a line of people --- the first person in line goes first and so on
 - **Keyword**: a structure in programming that is what it sounds like, a *key* *word*

<h2>Name and Pronunciation</h2>

There is a PEG/Packrat library *Neotoma* (a scientific classification for pack 
rats) -- Amotoen is Neotoma written in reverse, not that there is any particular 
quality or relationship shared between the two; I was just taking the shortest path 
to a unique name that might have been tangentially related to PEGs. You might even 
successfully define 'shortest path' as some stream-of-consciousness process.

I tend to pronounce Amotoen with a style that follows: Am-o-toe-n.
I tend to place emphasis on the 'toe'.

<h2>Reading Grammars</h2>

The number one goal for Amotoen is to have readable grammars.
Once you learn a few rules you should be able to read grammars.
 - Grammars are a Map with Keywords as keys, so it should be easy to lookup a rule
 - Vectors represent elements that must all match in sequence
 - Lists allow a wider range of defining how to treat elements in the list, all based on the first element in the list
  - A * (zero-or-more) requires zero or more of the rest of the list to match
  - A | (either) requires at least one of the rest of the list to match
  - A % (any-not) allows anything to match as long as it isn't matched by the rest of the list
  - An ! (not-predicate) checks to ensure that the rest of the list doesn't match

There are special types of lists that allow you to inject a function call into the process.
Lists that have an 'a' as the first element will contain an 'Aware Function'.
Lists that have an 'f' as the first element will contain a simpler 'Function'.

There are also special pre-provided keywords.
Use of the keyword ```:$``` means that the input should be at the end at that point.
Use of the keyword ```:.``` means that any character is accepted at that point.

<h2>Introduction to Reading Grammars (or, Don't Be Afraid)</h2>

Amotoen is a Clojure library that supports PEG style definitions of grammars that can produce parsers.
A parser can help you work with different inputs.
While there are academic papers available that rigorously define PEG, I've found
that PEGs, or **P**arsing **E**xpression **G**rammar(s), are best explained by the 
[related Wikipedia page](http://en.wikipedia.org/wiki/Parsing_expression_grammar).

There is a grammar provided in Amotoen for working with CSV files.
You should not be afraid of reading it or of working with CSV files.
Let's walk through a basic CSV grammar as provided in the com.lithinos.amotoen.grammars.csv package.
```
{
    :Document               [:Line '(* :Line) :$]
    :Line                   [:_* :Value '(* [:_* \, :_* :Value]) :_* '(* :EndOfLine)]
    :Value                  '(| [\" (* :DoubleQuotedValue) \"]
                                [\' (* :SingleQuotedValue) \']
                                (* :VanillaValue))
        :DoubleQuotedValue  '(| [\\ \"] [\\ \\] (% \"))
        :SingleQuotedValue  '(| [\\ \'] [\\ \\] (% \'))
        :VanillaValue       ['(! :EndOfLine) '(% \,)]
    :_*                     '(* :Whitespace)
    :Whitespace             '(| \space \tab)
    :EndOfLine              '(| \newline \return)
}
```
The above grammar reads as follows:
 - A Document is a Line followed by zero or more lines and then the end of the input
 - A Line, ignoring the chunks of whitespace, is a Value followed by zero or more pairs of commas and Values and then the end of the line
 - A Value is either some Double Quoted Values (wrapped in double quotes), some Single Quoted Values (wrapped in single quotes), or some Vanilla Values
 - A Double Quoted Value is either an escaped double quote, or an escaped backslash, or anything else as long as it isn't a double quote
 - A Single Quoted Value is either an escaped single quote, or an escaped backslash, or anything else as long as it isn't a single quote
 - A Vanilla Value, first making sure that we're not at the end of the line, is anything that isn't a comma

You should now be able to read the three rules that work with whitespace, the :Whitepsace, :EndOfLine, and the rule for zero-or-more :Whitespace.

<h2>Introduction to Using Grammars (or, It's Also Easy)</h2>

Using a grammar like the CSV one above can produce one of two types of outputs.
The more common output would be for a provider of a grammar to process the intermediate format and produce some kind of more relevant one,
The package com.lithinos.amotoen.grammars.csv provides a ```to-clj``` function that produces a Clojure data structure representing the CSV file.
An example of this format using the ```to-clj``` function and an input of just ```a,b,c\nx,y,z``` is as follows:
    ```[["a" "b" "c"] ["x" "y" "z"]]```

The less common output is an AST, or Abstract Syntax Tree.
You **shouldn't have to care about this first kind of output** unless you want to, since it's meant as an intermediate format.
An example of this format using the CSV grammar and an input of just ```a``` is as follows:
```
{
    :Document [
        {
            :Line [
                {:_* ()}
                {:Value {:VanillaValue [true \a]}}
                ()
                {:_* ()}
                ()
            ]
        }
        ()
        :$
    ]
}
```
If the more common output is not available, or you'd like to work with an AST, you can generate your own AST.
The following is only one of several different paths to producing an AST from a grammar and an input.
One way is to call the Amotoen function 'pegasus' and provide three arguments.
The first argument to pegasus is the key for the root rule in the grammar, the rule that should be run first.
The second argument is the grammar definition.
The third argument is something that fulfils the IAmotoen protocol, and the provided 'wrap-string' function will do just that for Strings.
Putting all these pieces together with the input from above of ```a``` you get:
    ```(pegasus :Document grammar (wrap-string "a"))```

Much more information can be found in the tests for CSV, the com.lithinos.amotoen.test.csv package.

<h2>Relation to clj-peg</h2>

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

<h2>Redefining Amotoen's Functionality</h2>

Aware Functions ('a') are slightly more complicated than simpler Functions ('f') but bring a **significantly** greater impact.
Simpler Functions must accept a single parameter and whatever they return are placed in the output at the point the function is called.
The single parameter given to simpler Functions is the result of Amotoen processing the remaining element in the list.
Aware Functions must accept two parameters and whatever they return is placed in the output at the point the function is called.
The two parameters given to Aware Functions are the grammar and the wrapped input.

Since Aware Functions are provided the grammar and the wrapped input, they can extend the functionality of Amotoen in any way.
If you dislike that the only terminals allowed by default are characters, then inject an Aware Function that allows something else.
Bits or Bytes or Regexs or whatever as terminals would be fairly simple to add.
If you dislike that there is a limited set of list types, then inject an Aware Function that allows something else.
An And-Predicate, a One-Or-More grouping, or a simpler combination of Not-Predicate guards followed by some :Body would be fairly simple to add.

An 'f' type list has three elements that matter and they occur in this order:
 1. The first element is an \f
 2. The second element is the function
 3. The third element is a :Body, as defined in the Amotoen grammar

An 'a' type list has only two elements that matter and they occur in this order:
 1. The first element is an \a
 2. The second element is the function

<h2>Amotoen Grammar Definition</h2>

The grammar for Amotoen grammars is:
```
{
    :_*             '(* :Whitespace)
    :_              [:Whitespace '(* :Whitespace)]
    :Grammar        [\{ :_* :Rule '(* [:_ :Rule]) :_* \}]
    :Rule           [:Keyword :_ :Body]
    :Keyword        [\: '(| :AmotoenSymbol :ProvidedSymbol)]
    :ProvidedSymbol '(| :EndOfInput :AcceptAnything)
    :EndOfInput     \$
    :AcceptAnything \.
    :Body           '(| :Keyword :Char :Grouping :NotPredicate :AnyNot :AwareFunction :Function)
    :Grouping       '(| :Sequence :Either :ZeroOrMore)
    :Sequence       [\[                     :_* :Body '(* [:_* :Body])  :_* \]]
    :Either         [\( \|                  :_  :Body '(* [:_* :Body])  :_* \)]
    :NotPredicate   [\( \!                  :_  :Body                   :_* \)]
    :ZeroOrMore     [\( \*                  :_  :Body                   :_* \)]
    :AnyNot         [\( \%                  :_  :Body                   :_* \)]
    :AwareFunction  [\( \a :_ :CljReaderFn                              :_* \)]
    :Function       [\( \f :_ :CljReaderFn  :_  :Body                   :_* \)]
    :CljReaderFn    [\# \< '(% \>) '(* (% \>)) \>]
    :Whitespace                 '(| \space \newline \return \tab \,)
    :Char                       [\\ (list '| (pegs "tab") (pegs "space") (pegs "newline") (pegs "return") '(% \space))]
    :AmotoenSymbol              [:NonNumericCharacter '(* :AlphanumericCharactersPlus)]
    :NonNumericCharacter        (list '% (lpegs '| "0123456789"))
    :AlphanumericCharactersPlus (lpegs '| "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789:/*+!-_?.")
}
```
<h2>Recent Improvements</h2>

You can now supply a 'custom collapse' function to elements like '\*.
Since the switch to a more character-based process, there have been annoying structures that 
represent little other than a set of characters that could better serve reduced to a string.

As an example:
 - Some function named 'custom-collapse' set to
    ```#(apply str %)```
 - Some grammar 'g' set to
    ```{:S [(list 'f custom-collapse (pegs "abcabc"))]}```
 - Some input 'i' set to 
    ```"abcabc"```
 - An invocation like...
    ```(pegasus :S g (gen-ps i))```
 - ... should return...
    ```{:S ["abcabc"]}```

Without supplying a custom collapse function:
 - Some grammar 'g' set to
    ```{:S (pegs "abcabc")}```
 - Other things alike, the result should be
    ```{:S [\a \b \c \a \b \c]}```

Another example:
 - Some function named 'custom-collapse' set to
    ```#(apply str %)```
 - Some grammar 'g' set to
    ```{:S [(list 'f custom-collapse '(* (lpegs '| "abc")))]}```
 - Some input 'i' set to 
    ```"aabbcc"```
 - An invocation like...
    ```(pegasus :S g (gen-ps i))```
 - ... should return...
    ```{:S ["aabbcc"]}```

Without supplying a custom collapse function:
 - Some grammar 'g' set to
    ```{:S [(list '* (lpegs '| "abc"))]}```
 - Other things alike, the result should be
    ```{:S [(\a \a \b \b \c \c)]}```


