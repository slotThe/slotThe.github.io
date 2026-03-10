---
title: Writing a JSON Parser in BQN
date: 2025-10-08
last-modified: 2025-10-18
tags: BQN, array-lang
og-description: Writing a JSON parser in BQN in 24 lines of code.
---

At least, for some vague definition of JSON.<!--more-->
I will restrict myself to the following underspecified subset,[^18]
so that this will not be a 20'000-word post:

+ Only (positive) integers.
+ ASCII only and only `\`-based escaping.
+ No constants.
+ Absolutely no input validation—we'll try to parse the wildest things.

Some of those are not difficult to add:
adding negative integers is a 4-character change,
and other number formats are not much harder.
Constants are a fun exercise for the reader, and also useful for JSON in the wild.
Input validation is not difficult per se,
but seemed a bit too tedious for this blog post to me.
I'm staying away from Unicode though,
see [bqn-libs/json.bqn][bqn-libs:json] for that.
In general, I do not claim originality for any line of code in this post<!--
-->—this is more or less my attempt to understand [bqn-libs/json.bqn][bqn-libs:json] to a reasonable degree.

This article assumes that you are already a bit familiar with BQN as a language,
or know some amount of array programming and are willing to look up unknown symbols.
If not, I recommend starting with the official [tutorials](https://mlochbaum.github.io/BQN/tutorial/index.html).
I've also written about my experience of
[using BQN](https://tony-zorman.com/posts/aoc-in-bqn.html)
for Advent of Code,
a post which certainly contains some amount of written words that talk about the language.
I'll recall the basic tricks that one uses in array-oriented parsing,
though if you feel like I'm going too fast perhaps it's a good idea to peruse my post about writing a
[CSV parser](https://tony-zorman.com/posts/bqn-csv.html).[^9]

# Lists of numbers

We'll start with simply trying to parse lists of integers.
Can't be that hard, right?
Additionally<!--
-->—and this is where *array-oriented* starts to come into play—<!--
-->we want to handle as much as possible using flat arrays.
This is both faster and way cooler.
In particular, this will result in a parser that has essentially no branching to speak of:
everything is just executed in order, line by line.

Motivated by an Advent of Code problem,
[Sylvia](https://saltysylvi.github.io/blog/index.html)
has already written a
[fantastic article](https://saltysylvi.github.io/blog/parsing-nested-lists-in-bqn.html)
about this exact type of thing,
including a comparison with two other approaches:
recursive descent, as well as a more nested array-style parser.[^8]
I will follow the same approach for this part of the parser;
if you've already read—and understood—Sylvia's post, feel free to skip to [Adding strings](#adding-strings).

Assume we have a list of numbers like the following:[^10]

``` bqn
    inp ← "[12,34,5,678,910]"
"[12,34,5,678,910]"
```

We'll follow standard practise by first tokenising—lexing—the string,
and then parsing the tokens.

## Tokenisation

Brackets can stay as they are,
each number will be replaced by a single `0`,
and thus all commas can be removed.
We'll then return a two element array,
the first entry containing the tokenised array,
and the second one being all of the numbers.

``` bqn
    Tokenise inp
⟨ "[00000]" ⟨ 12 34 5 678 910 ⟩ ⟩
```

The first thing we have to do is to get the start of each number,
and save that in a variable `ns`,
while a mask of all numbers will be called `n`.

``` bqn
    '0'+↕10
"0123456789"
    inp∊('0'+↕10)
⟨ 0 1 1 0 1 1 0 1 0 1 1 1 0 1 1 1 0 ⟩
    »⊸< inp∊('0'+↕10)
⟨ 0 1 0 0 1 0 0 1 0 1 0 0 0 1 0 0 0 ⟩
    ns ← »⊸< n←inp∊('0'+↕10)
⟨ 0 1 0 0 1 0 0 1 0 1 0 0 0 1 0 0 0 ⟩
    n         # For comparison
⟨ 0 1 1 0 1 1 0 1 0 1 1 1 0 1 1 1 0 ⟩
    inp ≍ ns  # Visualise ns by stacking inp on top of it
┌─
╵ '[' '1' '2' ',' '3' '4' ',' '5' ',' '6' '7' '8' ',' '9' '1' '0' ']'
   0   1   0   0   1   0   0   1   0   1   0   0   0   1   0   0   0
                                                                     ┘
```

While it's not immediately obvious,
defining `ns` is useful because we can create a depth scan of all numbers,
where the start of each new number increases the depth by one.

``` bqn
    +`ns
⟨ 0 1 1 1 2 2 2 3 3 4 4 4 4 5 5 5 5 ⟩
    inp ≍ +`ns
┌─
╵ '[' '1' '2' ',' '3' '4' ',' '5' ',' '6' '7' '8' ',' '9' '1' '0' ']'
   0   1   1   1   2   2   2   3   3   4   4   4   4   5   5   5   5
                                                                     ┘
```

By supplementing this with `n`,
we obtain sufficient information to *group* the string!

``` bqn
    n×+`ns                                    # Stencil out the numbers
⟨ 0 1 1 0 2 2 0 3 0 4 4 4 0 5 5 5 0 ⟩
    1-˜ n×+`ns                                # Kill irrelevant part
⟨ ¯1 0 0 ¯1 1 1 ¯1 2 ¯1 3 3 3 ¯1 4 4 4 ¯1 ⟩
    (1-˜ n×+`ns)⊔inp                          # Grouping
⟨ "12" "34" "5" "678" "910" ⟩
    •ParseFloat¨ (1-˜ n×+`ns)⊔inp             # Parsing
⟨ 12 34 5 678 910 ⟩
    (10⊸×⊸+˜´∘⌽-⟜'0')¨ (1-˜ n×+`ns)⊔inp       # Or: manual parsing
⟨ 12 34 5 678 910 ⟩
```

Now all that's left in the tokenisation step
is to create the string `"[00000]"` from the input.
Since we know all other tokens,
this can be achieved with a combination of *replicate*, *indices*, and *under*:

``` bqn
    ts ← ns ∨ inp∊"[]"                # Tokens are numbers and brackets
⟨ 1 1 0 0 1 0 0 1 0 1 0 0 0 1 0 0 1 ⟩
    ts/inp                            # Just the tokens of the string
"[13569]"
    ns/inp                            # Number beginnings
"13569"
    '0'¨⌾(ns⊸/)inp                    # Make them zero
"[02,04,0,078,010]"
    ts/'0'¨⌾(ns⊸/)inp                 # Choose just the tokens
"[00000]"
```

That'll do!
We can put all of this into a function now:[^1]

``` bqn
Tokenise ← {
  ns ← »⊸< n←𝕩∊'0'+↕10           # Number beginnings
  nr ← •ParseFloat¨(1-˜n×+`ns)⊔𝕩 # Numbers to return
  ts ← ns∨𝕩∊"[]"                 # Tokens
  ⟨ts/'0'¨⌾((/ns)⊸⊏)𝕩, nr⟩
}
```

This of course also works with nested lists,
giving us a flat representation in `nr`:

``` bqn
    Tokenise "[1,[2,[3],4],[5,6]]"
⟨ "[0[0[0]0][00]]" ⟨ 1 2 3 4 5 6 ⟩ ⟩
```

Using the "blueprint" `"[0[0[0]0][00]]"`,
the main job of the parser will be to turn the flat representation of `nr` back into a nested list.

## Parsing

The end result we want looks like this:

``` bqn
    Parse "[1,[2,[3]],4]"
⟨ 1 ⟨ 2 ⟨ 3 ⟩ ⟩ 4 ⟩
```

For this post, let's define a function to compactly display some text,
so that I can use some more complicated arguments without *couple* taking up too much space.

``` bqn
    DP ← { 𝕨≍'0'+𝕩 }
(function block)
    "abc" DP ⟨1,2,3⟩
┌─
╵"abc
  123"
      ┘
```

The first thing we need to do is to get a depth ordering of the list;
i.e., make an opening bracket increase and have a closing one decrease the depth by one.[^11]

``` bqn
    inp←"[1,[2,[3,4]],5,[6,7],8]"
"[1,[2,[3,4]],5,[6,7],8]"
    ts‿nums ← Tokenise inp # Tokens and numbers
⟨ "[0[0[00]]0[00]0]" ⟨ 1 2 3 4 5 6 7 8 ⟩ ⟩
    o ← '['=ts             # Opening brackets
⟨ 1 0 1 0 1 0 0 0 0 0 1 0 0 0 0 0 ⟩
    c ← ']'=ts             # Closing brackets
⟨ 0 0 0 0 0 0 0 1 1 0 0 0 0 1 0 1 ⟩
    ts DP +`o-c
┌─
╵"[0[0[00]]0[00]0]
  1122333211222110"
                   ┘
```

We can obtain a depth ordering of the list by turning ``+`o-c`` into a permutation with *grade up*
and then indexing into the tokens with *select*.

``` bqn
    d ← ⍋+`o-c # Depth ordering
⟨ 15 0 1 8 9 13 14 2 3 7 10 11 12 4 5 6 ⟩
    d⊏ts
"][0]0]0[0][00[00"
```

This is pretty hard to read at first (and even later),
but it's really nothing more than a breadth-first ordering of the input.
Since `inp` is defined as `"[1,[2,[3,4]],5,[6,7],8]"`
and so in particular only contains one-digit numbers,
it's perhaps more instructive to look at

``` bqn
    d⊏(','⊸≠)⊸/inp
"][1]5]8[2][67[34"
```

Drawing a picture often helps:

\begin{tikzpicture}{BFS representation of "][1]5]8[2][67[34" as a tree.}
  \begin{pgfonlayer}{nodelayer}
      \node [style=whitedot] (0) at (0, 5) {]};
      \node [style=whitedot] (1) at (0, 4.5) {[};
      \node [style=whitedot] (2) at (-2, 3.5) {1};
      \node [style=whitedot] (3) at (-1, 3.5) {]};
      \node [style=whitedot] (4) at (0, 3.5) {5};
      \node [style=whitedot] (5) at (1, 3.5) {]};
      \node [style=whitedot] (6) at (2, 3.5) {8};
      \node [style=whitedot] (7) at (-1, 3) {[};
      \node [style=whitedot] (10) at (1, 3) {[};
      \node [style=whitedot] (11) at (0.5, 2.25) {6};
      \node [style=whitedot] (12) at (1.5, 2.25) {7};
      \node [style=whitedot] (13) at (-1.5, 2.25) {2};
      \node [style=whitedot] (14) at (-0.5, 2.25) {]};
      \node [style=whitedot] (15) at (-0.5, 1.75) {[};
      \node [style=whitedot] (16) at (-1, 1) {3};
      \node [style=whitedot] (17) at (0, 1) {4};
  \end{pgfonlayer}
  \begin{pgfonlayer}{edgelayer}
      \draw (0) to (1);
      \draw (1) to (2);
      \draw (1) to (3);
      \draw (1) to (4);
      \draw (1) to (5);
      \draw (1) to (6);
      \draw (3) to (7);
      \draw (5) to (10);
      \draw (10) to (11);
      \draw (10) to (12);
      \draw (15) to (16);
      \draw (15) to (17);
      \draw (7) to (13);
      \draw (7) to (14);
      \draw (14) to (15);
  \end{pgfonlayer}
\end{tikzpicture}

So `]` indicates that a new sublist be placed at a certain slot,
and `[` is the start of such a sublist.
Filling in the tree from the string proceeds from left to right,
and every time we encounter a `[` we look for the "oldest" `]` that still isn't filled, and start there.

We now assign each sublist the depth in this tree representation.
This works much like the ``+`o-c`` trick above,
only we don't need to care about closing parentheses<!--
-->—they only show up when we are going a level deeper anyways.

``` bqn
    '['=d⊏ts
⟨ 0 1 0 0 0 0 0 1 0 0 1 0 0 1 0 0 ⟩
    l ← (⍋d)⊏+`'['=d⊏ts             # Nesting
⟨ 1 1 2 2 4 4 4 2 1 1 3 3 3 1 1 0 ⟩
    ts DP l
┌─
╵"[0[0[00]]0[00]0]
  1122444211333110"
                   ┘
```

Compare this with the above tree.
One important detail is that we index with `⍋d` and not just `d`:
since `d` is a permutation already, we can invert it with another call to *grade up*.
This effectively allows us to translate back from the BFS ordering of `d⊏ts`,
into the "ordinary" ordering that we started with.

This is a lot to take in;
I would encourage you to play with these ideas for a while, until you're comfortable with them.

---

Now that we know in which order to fill the sublists,
it's about time that we figure out which values to use for that very filling.

``` bqn
    ln ← ts∊"0]"                 # Literals and nesting
⟨ 0 1 0 1 0 1 1 1 1 1 0 1 1 1 1 1 ⟩
    ts DP ln
┌─
╵"[0[0[00]]0[00]0]
  0101011111011111"
                   ┘
    (ln/ts='0') + 2×ln/c
⟨ 1 1 1 1 2 2 1 1 1 2 1 2 ⟩
    vi ← ⍋⍋ (ln/ts='0') + 2×ln/c # Value indices
⟨ 0 1 2 3 8 9 4 5 6 10 7 11 ⟩
    (ln/ts) ≍ vi
┌─
╵ '0' '0' '0' '0' ']' ']' '0' '0' '0' ']' '0' ']'
   0   1   2   3   8   9   4   5   6   10  7   11
                                                  ┘
    inp
"[1,[2,[3,4]],5,[6,7],8]"
```

We separate out the literals,
as well as the "nesting indicator" `]`,
and then produce a list of "value indices"
that should indicate the order in which the final output is to be filled.
We use the
[ordinals pattern](https://mlochbaum.github.io/BQN/doc/order.html#ordinals)
`⍋⍋` to turn `ln` into something we can actually use to index into `ts`.
However, there is a problem:
the filling of the actual literals will be from left to right,
so the order is fine already,
but the closing brackets are all over the place.
For example, the second `]`, assigned to index `9`, is associated to the sublist `[6,7]` in the original input.
Since we will construct the final list bottom-up,
this should come after the list `[2,[3,4]]`,
the closing bracket of which is however assigned to index `10`.

To fix this, we use the nesting variable `l` defined above<!--
-->—there, we've already associated a sublist to its depth in the breadth-first representation.
Looking at `l` again, we can find the index of the sublist that a given `]` is closing by looking at the index of the element in front of it:

``` bqn
    ts DP l
┌─
╵"[0[0[00]]0[00]0]
  1122444211333110"
                   ┘
    c/»l
⟨ 4 2 3 1 ⟩
```

Since the list with the highest index above will be constructed first,
we have to invert these numbers.
Then, we just have to add these to the rest of the literals.

``` bqn
    c/»l
⟨ 4 2 3 1 ⟩
    ≠⊸- c/»l          # Deeply nested lists are constructed *first*
⟨ 0 2 1 3 ⟩
    (≠nums)+ ≠⊸- c/»l # Indices should not clash with literals
⟨ 8 10 9 11 ⟩
    (↕≠nums)∾ (≠nums)+ ≠⊸- c/»l # Add to end
⟨ 0 1 2 3 4 5 6 7 8 10 9 11 ⟩
    vi ↩ vi⊏(↕≠nums)∾ (≠nums)+ ≠⊸- c/»l # Fix vi by indexing this new list
⟨ 0 1 2 3 8 10 4 5 6 9 7 11 ⟩
```

This gives us fixed indices:

``` bqn
    (ln/ts) ≍ vi
┌─
╵ '0' '0' '0' '0' ']' ']' '0' '0' '0' ']' '0' ']'
   0   1   2   3   8   10  4   5   6   9   7   11
                                                  ┘
```

This means we first construct `[3,4]`, then `[2,[3,4]]`, then `[6,7]`, and finally the whole list `[1,[2,[3,4]],5,[6,7],8]`.
For this, we simply have to *group* the indices of `vi` accordingly.

``` bqn
    vi
⟨ 0 1 2 3 8 10 4 5 6 9 7 11 ⟩
    ln/l # Since vi also only contains indices for literals and nesting
⟨ 1 2 4 4 2 1 1 3 3 1 1 0 ⟩
    (ln/l)⊔vi
⟨ ⟨ 11 ⟩ ⟨ 0 10 4 9 7 ⟩ ⟨ 1 8 ⟩ ⟨ 5 6 ⟩ ⟨ 2 3 ⟩ ⟩
    1-˜ln/l # Drop final ] for the whole list
⟨ 0 1 3 3 1 0 0 2 2 0 0 ¯1 ⟩
    ⌽(1-˜ln/l)⊔vi # Reverse to build deeply nested lists sooner
⟨ ⟨ 2 3 ⟩ ⟨ 5 6 ⟩ ⟨ 1 8 ⟩ ⟨ 0 10 4 9 7 ⟩ ⟩
```

Now all that is left is to build the whole list recursively.
We start with the list of literals,
construct the next list, and add it to the end of the list of literals.
This way, the later indices `8`, `9`, and `10` will refer to these sublists.

``` bqn
    ⌽(1-˜ln/l)⊔vi
⟨ ⟨ 2 3 ⟩ ⟨ 5 6 ⟩ ⟨ 1 8 ⟩ ⟨ 0 10 4 9 7 ⟩ ⟩
    nums
⟨ 1 2 3 4 5 6 7 8 ⟩
    2‿3⊏nums
⟨ 3 4 ⟩
    nums ∾↩ <2‿3⊏nums
⟨ 1 2 3 4 5 6 7 8 ⟨ 3 4 ⟩ ⟩
    nums ∾↩ <5‿6⊏nums
⟨ 1 2 3 4 5 6 7 8 ⟨ 3 4 ⟩ ⟨ 6 7 ⟩ ⟩
    nums ∾↩ <1‿8⊏nums
┌─
· 1 2 3 4 5 6 7 8 ⟨ 3 4 ⟩ ⟨ 6 7 ⟩ ⟨ 2 ⟨ 3 4 ⟩ ⟩
                                                ┘
    nums ∾↩ <0‿10‿4‿9‿7⊏nums
┌─
· 1 2 3 4 5 6 7 8 ⟨ 3 4 ⟩ ⟨ 6 7 ⟩ ⟨ 2 ⟨ 3 4 ⟩ ⟩ ┌─
                                                · 1 ⟨ 2 ⟨ 3 4 ⟩ ⟩ 5 ⟨ 6 7 ⟩ 8
                                                                              ┘
                                                                                ┘
    ¯1⊑nums
┌─
· 1 ⟨ 2 ⟨ 3 4 ⟩ ⟩ 5 ⟨ 6 7 ⟩ 8
                              ┘
```

More concisely:

``` bqn
    res ← nums # Mutation is evil
⟨ 1 2 3 4 5 6 7 8 ⟩
    {res ∾↩ <𝕩⊏res ⋄@}¨ ⌽(1-˜ln/l)⊔vi

    ¯1⊑res
┌─
· 1 ⟨ 2 ⟨ 3 4 ⟩ ⟩ 5 ⟨ 6 7 ⟩ 8
                              ┘
```

One tiny change has to be made to the building of the sublists:
if our input is `"[]"`, then `1-˜ln/l` will be `⟨¯1⟩`,
so the list as a whole will be dropped!
We can hackily fix this by always appending the total number of closing brackets to the back of the list via `(+´c)∾˜`.
If the list is empty, then this will accommodate for that case.
Otherwise, the left argument to *group* will be one element longer than the right one.
This is not a problem, however, because that's actually a feature[^12] of the function:
in that case, the last element specified the minimum length of the result.

``` bqn
    0‿¯1‿1‿4 ⊔ "abc" # Padding
⟨ "a" "c" ⟨⟩ ⟨⟩ ⟩
```

Putting everything together:

``` bqn
Parse ← {
  ts‿nums ← Tokenise 𝕩                 # Tokens and numbers
  o ← '['=ts                           # Opening brackets
  c ← ']'=ts                           # Closing brackets
  d ← ⍋+`o-c                           # Depth ordering
  ln ← ts∊"0]"                         # Literals and nesting
  l ← (⍋d)⊏+`'['=d⊏ts                  # Sublist depth
  vi ← ⍋⍋ (ln/ts='0') + 2×ln/c         # Value indices
  r ← nums                             # Result
  vi ⊏↩ (↕≠nums)∾(≠nums)+ ≠⊸- c/»l     # Fix value indices
  {r ∾↩ <𝕩⊏r ⋄@}¨ ⌽((+´c)∾˜1-˜ln/l)⊔vi # Build result
  ¯1⊑r
}
```

Works like a charm:

``` bqn
    Parse "[1,[2,[3,4]],5,[6,7],8]"
┌─
· 1 ⟨ 2 ⟨ 3 4 ⟩ ⟩ 5 ⟨ 6 7 ⟩ 8
                              ┘
    Parse "[]"
⟨⟩
    Parse "[1, [42, [[0], 2, 3]], 30]"
┌─
· 1 ┌─                   30
    · 42 ⟨ ⟨ 0 ⟩ 2 3 ⟩
                       ┘
                            ┘
    Parse "[1,2,[],[3,4,[5,6]],7,8,[9,[],10]]"
┌─
· 1 2 ⟨⟩ ⟨ 3 4 ⟨ 5 6 ⟩ ⟩ 7 8 ⟨ 9 ⟨⟩ 10 ⟩
                                         ┘
```

# Adding strings

I'd like to add one more literal,
so that we get a feeling for how to do that sort of extension;
strings sound like a good idea.

First, the lexer: getting the strings and their beginnings works almost the same as in the numbers case above.[^13]

``` bqn
    inp ← "[1,""a"",2,""b""]"
"[1,""a"",2,""b""]"
    s ← ≠`'"'=inp             # strings
⟨ 0 0 0 1 1 0 0 0 0 1 1 0 0 ⟩
```

Before we move on, let's slightly change the definition of `DP`:
for syntax highlighting reasons, I'd like quotes to occupy a single character that is not the quote character itself.
Let's just substitute `'` for `"` in the left argument,
which should fix this issue and hopefully still be clear enough.

``` bqn
    DP ← {((@+39)¨⌾((𝕨='"')⊸/)𝕨) ≍ '0'+𝕩}
(function block)
    inp DP s
┌─
╵"[1,'a',2,'b']
  0001100001100"
                ┘
    sb ← »⊸< s                # string beginnings
⟨ 0 0 0 1 0 0 0 0 0 1 0 0 0 ⟩
    inp DP sb
┌─
╵"[1,'a',2,'b']
  0001000001000"
                ┘
```

Selecting the strings also works the same way;
do a sum *scan* on the string beginnings, punch out the actual strings, and then *group*.

``` bqn
    1-˜s×+`sb
⟨ ¯1 ¯1 ¯1 0 0 ¯1 ¯1 ¯1 ¯1 1 1 ¯1 ¯1 ⟩
    +`sb
⟨ 0 0 0 1 1 1 1 1 1 2 2 2 2 ⟩
    1-˜s×+`sb
⟨ ¯1 ¯1 ¯1 0 0 ¯1 ¯1 ¯1 ¯1 1 1 ¯1 ¯1 ⟩
    (1-˜s×+`sb)⊔inp
⟨ """a" """b" ⟩
    1↓¨ (1-˜s×+`sb)⊔inp
⟨ "a" "b" ⟩
```

Actually, you can be a little bit smarter here;
instead of dropping the first element of every string,
we can instead make sure that they are never selected by *group*
by comparing when `s` is bigger than `sb`:
this will only be the case if one is truly inside of a string.

``` bqn
    (s>sb)×+`sb               # Same as (s∧¬sb)×+`sb
⟨ 0 0 0 0 1 0 0 0 0 0 2 0 0 ⟩
    (1-˜(s>sb)×+`sb)⊔inp
⟨ "a" "b" ⟩
```

Putting this together in our tokeniser,
we now also need to make sure that we ignore everything that's happening inside of a string.
To do that, we can introduce an "exclusion" variable
`ex ← s∨𝕩∊@+9‿10‿13‿32`
and thread it through to the respective definitions.[^5]

``` bqn
Tokenise ← {
  sb ← »⊸< s←≠`'"'=𝕩             # String beginnings
  sr ← (1-˜(s>sb)×+`sb)⊔𝕩        # Strings to return

  ex ← s∨𝕩∊@+9‿10‿13‿32          # Exclude whitespace and strings
  nb ← »⊸< n← (¬ex)∧ 𝕩∊'0'+↕10   # Number beginnings
#             ^^^^^^
  nr ← •ParseFloat¨(1-˜n×+`nb)⊔𝕩 # Numbers to return

  ts ← sb∨(¬ex)∧nb∨𝕩∊"[]"        # Tokens
#         ^^^^^^
  ⟨ts/ '0'¨⌾(nb⊸/) 𝕩, nr, sr⟩
}
```

``` bqn
    Tokenise "[1,""a"",2,""b""]"
⟨ "[0""0""]" ⟨ 1 2 ⟩ ⟨ "a" "b" ⟩ ⟩
    Tokenise "[1,""a,2,b""]"
⟨ "[0""]" ⟨ 1 ⟩ ⟨ "a,2,b" ⟩ ⟩
```

## Parsing

Adjusting the parser is actually quite easy here:
we just have to add quotes to the allowed literals,
make the value indices aware of them,
and also feed them into the initial value of the result;
no further changes necessary.

``` bqn
Parse ← {
  ts‿nums‿strs ← Tokenise 𝕩                    # Tokens, numbers, and strings
#        ^^^^^
  o ← '['=ts                                   # Opening, closing, depth
  c ← ']'=ts                                   # Closing brackets
  d ← ⍋+`o-c                                   # Depth ordering
  ln ← ts∊"""0]"                               # Literals and nesting
#          ^^
  l ← (⍋d)⊏+`'['=d⊏ts                          # Sublist depth
  vi ← ⍋⍋ (ln/ts='0') + (2×ln/ts='"') + 3×ln/c # Value indices
#                       ^^^^^^^^^^^^^
  r ← nums∾strs                                # Result
#         ^^^^^
  vi ⊏↩ (↕≠r)∾(≠r)+ ≠⊸- c/»l                   # Fix value indices
  {r ∾↩ <𝕩⊏r ⋄@}¨ ⌽((+´c)∾˜1-˜ln/l)⊔vi         # Build result
  ¯1⊑r
}
```

## Escaping

To make things a little bit—though not too—interesting, let's add some easy escaping.
By this I exclusively mean backslash-based escaping:
every time we encounter a backslash inside of a string,
we treat the following character (e.g., a quote) literally.
Thinking about semantics a bit, if we have a sequence of backslashes like `\\\\\`,
we want to alternate between treating them as the escapist and the escapee.
One way to do this is with a less-than *scan*:

``` bqn
    inp ← "[1,""a"",""b_\\\""\\_c""]"
"[1,""a"",""b_\\\""\\_c""]"
    '\'=inp
⟨ 0 0 0 0 0 0 0 0 0 0 1 1 1 0 1 1 0 0 0 0 ⟩
    <`'\'=inp
⟨ 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 ⟩
    inp DP <`'\'=inp
┌─
╵"[1,'a','b_\\\'\\_c']
  00000000001010100000"
                       ┘
```

We effectively check if the previous character of the string was escaped, but in a left-to-right fashion
(as in, earlier choices on the left affect those on the right).
To get the escaped values themselves, we can now simply *nudge* the string to the right.

``` bqn
    inp DP »<`'\'=inp
┌─
╵"[1,'a','b_\\\'\\_c']
  00000000000101010000"
                       ┘
```

Finally, when selecting the strings, we just need to exclude the escaped quotes:

``` bqn
    e ← »<`'\'=inp     # Escapes
⟨ 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 0 0 0 ⟩
    s ← ≠`(¬e)∧'"'=inp # Strings are in quotes that are not escaped
⟨ 0 0 0 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 0 0 ⟩
    sb ← »⊸< s
⟨ 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 ⟩
    (1-˜(s>sb)×+`sb)⊔inp
⟨ "a" "b_\\\""\\_c" ⟩
```

As we are dealing with Boolean masks,
alternatively to `(¬e)∧'"'=inp` one could also write the shorter,
though slightly more obscure,
`e<'"'=inp`.
All in all, we have

``` bqn
Tokenise ← {
  e  ← »<`'\'=𝕩                  # Escapes
  sb ← »⊸< s← ≠`(¬e)∧ '"'=𝕩      # String beginnings
  sr ← (1-˜(s>sb)×+`sb)⊔𝕩        # Strings to return

  ex ← s∨ 𝕩∊@+9‿10‿13‿32         # Exclude whitespace and strings
  nb ← »⊸< n← (¬ex)∧ 𝕩∊'0'+↕10   # Number beginnings
  nr ← •ParseFloat¨(1-˜n×+`nb)⊔𝕩 # Numbers to return

  ts ← sb∨(¬ex)∧nb∨𝕩∊"[]"        # Tokens
  ⟨ts/ '0'¨⌾(nb⊸/) 𝕩, nr, sr⟩
}
```

``` bqn
    Tokenise "[1,""a,2,b""]"
⟨ "[0""]" ⟨ 1 ⟩ ⟨ "a,2,b" ⟩ ⟩
    Tokenise "[1,""a"",""b_\\\""\\_c"",2]"
⟨ "[0""""0]" ⟨ 1 2 ⟩ ⟨ "a" "b_\\\""\\_c" ⟩ ⟩
```

The parser doesn't need to be adjusted at all this time and just works:

``` bqn
    Parse "[1,""a"",""b_\\\""\\_c"",2]"
⟨ 1 "a" "b_\\\""\\_c" 2 ⟩
    Parse "[1,[2,[""[x,10]"",4]],5,[""a"",7],8]"
┌─
· 1 ⟨ 2 ⟨ "[x,10]" 4 ⟩ ⟩ 5 ⟨ "a" 7 ⟩ 8
                                       ┘
```

# Objects and their ilk

Onto the second big type we have to support: objects.
They should look like

```
{"a": 1, "b": ["c", "d"]}
```

and so on.

This time, lexing is the easy part—we merely have to add curly braces to our list of tokens.
Actually, to keep things visually comprehensible,
let's go back on what I said at the beginning
and do the same for the comma as the array separator,
and the colon as the key–value separator.

``` bqn
Tokenise ← {
  e  ← »<`'\'=𝕩                  # Escapes
  sb ← »⊸< s← ≠`(¬e)∧ '"'=𝕩      # String beginnings
  sr ← (1-˜(s>sb)×+`sb)⊔𝕩        # Strings to return

  ex ← s∨ 𝕩∊@+9‿10‿13‿32         # Exclude whitespace and strings
  nb ← »⊸< n← (¬ex)∧ 𝕩∊'0'+↕10   # Number beginnings
  nr ← •ParseFloat¨(1-˜n×+`nb)⊔𝕩 # Numbers to return

  ts ← sb∨(¬ex)∧nb∨𝕩∊"[]{},:"    # Tokens
#                       ^^^^
  ⟨ts/ '0'¨⌾(nb⊸/) 𝕩, nr, sr⟩
}
```

``` bqn
    Tokenise "{""a"": 123, ""bcde"": [1,""42"",30]}"
⟨ "{"":0,"":[0,"",0]}" ⟨ 123 1 30 ⟩ ⟨ "a" "bcde" "42" ⟩ ⟩
```

Looks good.

## Parsing

Since the parser will change quite a bit,
it is perhaps more instructive to build it up from scratch again.
Up to getting the depth, things don't really change much.

``` bqn
    inp ← "{""a"": 1, ""b"": [""1"",2,{""a"":2}]}"
"{""a"": 1, ""b"": [""1"",2,{""a"":2}]}"
    ts‿nums‿strs ← Tokenise inp
⟨ "{"":0,"":["",0,{"":0}]}" ⟨ 1 2 2 ⟩ ⟨ "a" "b" "1" "a" ⟩ ⟩
    o ← ts∊"{["  # Open
⟨ 1 0 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 ⟩
    c ← ts∊"]}"  # Close
⟨ 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 ⟩
    cl ← ':'=ts  # Colon
⟨ 0 0 1 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 ⟩
    d ← ⍋+`o-c   # Depth
⟨ 18 0 1 2 3 4 5 6 17 7 8 9 10 11 16 12 13 14 15 ⟩
    td ← d⊏ts    # Tokens by depth
"}{"":0,"":]["",0,}{"":0"
```

Since we now have two different "container" types,
we also need to be careful about how "nesting" is defined—<!--
-->sometimes we may only want the delimiters of one container to increase the level of nesting.
Let's start with getting a mask of the start of all new sublists[^14],
which are now defined by an opening brace or bracket:

``` bqn
    s ← td∊"[{"  # Sublist starts
⟨ 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 ⟩
    td DP s
┌─
╵"}{':0,':][',0,}{':0
  0100000001000001000"
                      ┘
```

We also did the same thing in the original parser
when we defined the sublist depth ``l ← (⍋d)⊏+`'['=d⊏ts``,
only we didn't give `'['=d⊏ts`
(which now morphed to `(d⊏ts)∊"[{"` due to the additional container types) a name.
However, defining the sublist depth still works as before:

``` bqn
    n ← (⍋d)⊏+`s  # Nesting
⟨ 1 1 1 1 1 1 1 2 2 2 2 2 3 3 3 3 2 1 0 ⟩
    ts DP n
┌─
╵"{':0,':[',0,{':0}]}
  1111111222223333210"
                      ┘
```

Moving on, we can use the mask of sublists `s` to build a mask of sub-objects.

``` bqn
    s
⟨ 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 ⟩
    td DP s
┌─
╵"}{':0,':][',0,}{':0
  0100000001000001000"
                      ┘
    of ← s/'{'=td  # Filter of sublists that are objects
⟨ 1 0 1 ⟩
```

The `of` variable is to be read as something like "there are three sublists, the first and third of which are sub-objects".
This is good, but doesn't yet take object nesting into account<!--
-->—the second `1` in `of`,
being nested inside of the first object,
is actually at depth two and should be treated accordingly.
To fix this, we proceed as before and do a sum *scan* across `of`,
with a subsequent stencil to make sure we only select the objects.

``` bqn
    +`of
⟨ 1 1 2 ⟩
    on ← of × +`of  # Object nesting
⟨ 1 0 2 ⟩
    on ← +`⊸× of    # Object nesting (prettier!)
⟨ 1 0 2 ⟩
```

Next, let's get the keys of each object and *group* them by depth,
as this will make subsequent processing much easier.
That is, given an input like `{"a": 1, "b": [1, "c", {"a": 2}]}`,
we want to end up with `⟨⟨"a", "b"⟩, ⟨"a"⟩⟩`.
Actually, since strings can be values as well,
we probably want to separate those, too,
ending up with an array of the form `⟨⟨"c"⟩, ⟨"a", "b"⟩, ⟨"a"⟩⟩`.

Selecting the keys themselves is easy:
just go to the colons,
*nudge* them to consider the token before,
and *replicate* from all strings.

``` bqn
    cl
⟨ 0 0 1 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 ⟩
    ts DP «cl
┌─
╵"{':0,':[',0,{':0}]}
  0100010000000100000"
                      ┘
    ('"'=ts)/«cl
⟨ 1 1 0 1 ⟩
```

A `0` stands for "this string is a value",
and a `1` is for "this string is a key somewhere".
We already have a nesting variable `n`,
which however also increases the depth when hitting an array instead of an object.
If we had one only for objects, we could use that as a stencil to obtain a depth ordered list when combining it with the above.

There's (at least) two things we can do:
re-create `n` but only take into consideration objects,
or somehow use `n`, but punch holes into it for the arrays.
The first way is straightforward and essentially just copies the definition of `n`:

``` bqn
    (⍋d)⊏+`td='{'
⟨ 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 1 1 0 ⟩
    ts DP (⍋d)⊏+`td='{'
┌─
╵"{':0,':[',0,{':0}]}
  1111111111112222110"
                      ┘
    («cl)× (⍋d)⊏+`td='{'
⟨ 0 1 0 0 0 1 0 0 0 0 0 0 0 2 0 0 0 0 0 ⟩
    ('"'=ts)/ («cl)× (⍋d)⊏+`td='{'
⟨ 1 1 0 2 ⟩
```

However, this seems quite inelegant to me;
thankfully, punching holes into `n` is not as hard as it sounds.
We already have an array `on` that tells us about the object nesting,
with a `0` standing in for "this is the start of an array".
That means if we simply use `n` to *select* from `on`,
this gives us what we want![^6]

``` bqn
    ts DP n
┌─
╵"{':0,':[',0,{':0}]}
  1111111222223333210"
                      ┘
    n⊏0∾on
⟨ 1 1 1 1 1 1 1 0 0 0 0 0 2 2 2 2 0 1 0 ⟩
    ts DP n⊏0∾on
┌─
╵"{':0,':[',0,{':0}]}
  1111111000002222010"
                      ┘
    ('"'=ts) / («cl)×n⊏0∾on
⟨ 1 1 0 2 ⟩
```

All that's left is to *group* the given strings!

``` bqn
    (('"'=ts)/(«cl)×n⊏0∾on) ⊔ strs         # Keys
⟨ ⟨ "1" ⟩ ⟨ "a" "b" ⟩ ⟨ "a" ⟩ ⟩
    (('"'=ts)/(«cl)×(⍋d)⊏+`td='{') ⊔ strs  # Same result
⟨ ⟨ "1" ⟩ ⟨ "a" "b" ⟩ ⟨ "a" ⟩ ⟩
```

There is but a small wrinkle yet to fix:
if the object is empty, it might be that
``(('"'=ts)/(«cl)×(⍋d)⊏+`td='{')`` returns a nonsense answer,
which would yield an error when trying to *group* the strings.
This is much the same situation as before when we had to change `1-˜ln/l` to `(+´c)∾˜1-˜ln/l` during the construction of the final array.
The fix here is similar, since with `of` we have an obvious quantity, the *length* of which is always the length we want to end up with,[^15]
even when trying to parse `{}`.

``` bqn
    ks ← ((1+´of)∾˜ ('"'=ts)/(«cl)×n⊏0∾on)⊔strs # Keys, final definition
⟨ ⟨ "1" ⟩ ⟨ "a" "b" ⟩ ⟨ "a" ⟩ ⟩
```

Building up our initial list of values,
this now includes all numbers, as well as all strings that are not keys.
Conveniently, by the above argument the latter is exactly the first element of `ks`.

``` bqn
    vs ← nums ∾ ⊑ks # Initial values
⟨ 1 2 2 "1" ⟩
```

Building up the value indices works exactly the same as before;
the only complication now is that a definition of `ln ← ts∊"""0]"` for literals and nesting does not work anymore, as we need to exclude the strings that are keys.
What seems to work best is to specify what `ln` should *not* be:
a comma, a colon, any opening parenthesis, and its token should not occur immediately before a colon.

``` bqn
    ln ← ¬ («⊸∨cl)∨o∨','=ts                       # Literals and nesting
⟨ 0 0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 1 1 1 ⟩
    vi ← ⍋⍋ (ln/ts='0') + (2×ln/ts='"') + 3×ln/c  # Value indices
⟨ 0 3 1 2 4 5 6 ⟩
    vi ⊏↩ (↕≠vs)∾(≠vs)+ ≠⊸- c/»n                  # Fix value indices
⟨ 0 3 1 2 4 5 6 ⟩
```

Almost done—the only thing left is to build up the finished, nested, array.
This is also the time we have to decide upon the final representation of an object.
What turns out to be quite convenient for small-ish JSON<!--
-->—which, let's be honest, will be the only kind of JSON that `Parse` ever gets to see—<!--
-->is to use *rank* 2 arrays;
an object `{"a": 1}` gets parsed into `2‿1⥊"a"‿1`.
Further, when building the result we have to make sure to "recognise" when to pull values and sublists out of the recursively built up `vs`,
and when to use `ks` to get keys instead.
This can be achieved by taking `on` into consideration:

``` bqn
    ⌽((≠on)∾˜1-˜ln/n)⊔vi  # How to build which sublist
⟨ ⟨ 2 ⟩ ⟨ 3 1 4 ⟩ ⟨ 0 5 ⟩ ⟩
    ⌽on                   # Which sublist is an object?
⟨ 2 0 1 ⟩
    on ≍○⌽ ((≠on)∾˜1-˜ln/n)⊔vi
┌─
╵ 2     0         1
  ⟨ 2 ⟩ ⟨ 3 1 4 ⟩ ⟨ 0 5 ⟩
                         ┘
```

This means that the final shape is something like
`on BUILD¨○⌽ ((≠on)∾˜1-˜ln/l)⊔vi`,
where `BUILD` is a dyadic function,
taking as its left argument a number that's zero if we are dealing with a list or literal,
and bigger than zero if we have an object and want to select at least one key.
The right argument first *select*s the required sublist or value from `vs`,
and then refines this selection by picking possible keys out of `ks`.
Putting this into code:

``` bqn
    on {vs ∾↩ <𝕨Sel𝕩⊏vs ⋄@}¨○⌽ ((≠on)∾˜1-˜ln/l)⊔vi # Build result
```

All that's left to do is to write `Sel`.
Doing nothing when `𝕨` is zero can just be achieved with a
[header](https://mlochbaum.github.io/BQN/doc/block.html#block-headers).
If `𝕨` is a number, it represents the depth of the object, and directly corresponds with the indexing of `ks`,
so we can just *pick* the result and *couple* it to `𝕩` to make things a *rank* 2 array.[^16]

``` bqn
    Sel ← {0𝕊x: x; i𝕊x: (i⊑ks)≍x}  # Select
(function block)
    Sel ← ⊑⟜ks⊸≍⍟(0<⊣)             # Point-free and without blocks
⊑⟜⟨ ⟨ "1" ⟩ ⟨ "a" "b" ⟩ ⟨ "a" ⟩ ⟩⊸≍⍟(0<⊣)

# The object {"a": 2} is the first non-literal that's constructed.
    inp
"{""a"": 1, ""b"": [""1"",2,{""a"":2}]}"
    ⌽((≠on)∾˜1-˜ln/n)⊔vi
⟨ ⟨ 2 ⟩ ⟨ 3 1 4 ⟩ ⟨ 0 5 ⟩ ⟩
    ⌽on
⟨ 2 0 1 ⟩
    vs
⟨ 1 2 2 "1" ⟩
    2 Sel ⟨2⟩⊏vs
┌─
╵ "a"
   2
      ┘
```

And that's pretty much it—here are the last two lines, in all their glory:

``` bqn
    on {vs ∾↩ <𝕨Sel𝕩⊏vs ⋄@}¨○⌽ ((≠on)∾˜1-˜ln/n)⊔vi  # Build result

    ¯1⊑vs                                           # Get result
┌─
╵ "a" "b"
   1   ┌─
       · "1" 2 ┌─
               ╵ "a"
                  2
                     ┘
                       ┘
                         ┘
    inp
"{""a"": 1, ""b"": [""1"",2,{""a"":2}]}"
```

It works!

---

At the risk of repeating myself, let's look at the entire parser as well as the tokeniser that we've built over the course of this post.

``` bqn
Tokenise ← {
  e  ← »<`'\'=𝕩                  # Escapes
  sb ← »⊸< s← ≠`(¬e)∧ '"'=𝕩      # String beginnings
  sr ← (1-˜(s>sb)×+`sb)⊔𝕩        # Strings to return

  ex ← s∨ 𝕩∊@+9‿10‿13‿32         # Exclude whitespace and strings
  nb ← »⊸< n← (¬ex)∧ 𝕩∊'0'+↕10   # Number beginnings
  nr ← •ParseFloat¨(1-˜n×+`nb)⊔𝕩 # Numbers to return

  ts ← sb∨(¬ex)∧nb∨𝕩∊"[]{},:"    # Tokens
  ⟨ts/ '0'¨⌾(nb⊸/) 𝕩, nr, sr⟩
}

Parse ← {
  ts‿nums‿strs ← Tokenise 𝕩
  d  ← ⍋+`(o←ts∊"{[")-(c←ts∊"]}") # Closing, opening, depth
  td ← d⊏ts                       # Tokens by depth
  s  ← td∊"[{"                    # Sublist starts
  n  ← (⍋d)⊏+`s                   # Nesting

  of ← s/'{'=td                   # Filter of subsists that are objects
  on ← +`⊸× of                    # Object nesting
  cl ← ':'=ts                     # Colon
  ks ← ((1+´of)∾˜ ('"'=ts)/(«cl)×n⊏0∾on)⊔strs    # Keys

  vs ← nums ∾ ⊑ks                 # Initial values
  ln ← ¬ («⊸∨cl)∨o∨','=ts         # Literals and nesting
  vi ← ⍋⍋ (ln/ts='0') + (2×ln/ts='"') + 3×ln/c   # Value indices
  vi ⊏↩ (↕≠vs)∾(≠vs)+ ≠⊸- c/»n                   # Fix value indices

  Sel ← {(𝕨⊑ks)≍𝕩}⍟(0<⊣)          # Select
  on {vs ∾↩ <𝕨Sel𝕩⊏vs ⋄@}¨○⌽ ((≠on)∾˜1-˜ln/n)⊔vi # Build result
  ¯1⊑vs
}
```

I don't know about you,
but I think it's pretty incredible that one can write a parser for a reasonable subset[^7] of JSON,
using what are essentially only flat arrays of numbers,
and still have it be this compact.

# Conclusion

I can probably offer the same conclusion as in the
[CSV post](https://tony-zorman.com/posts/bqn-csv.html):

> Q: Is writing (and reading) a parser in an array language unnecessarily hard? \
> A: Yes.
>
> Q: Is it the most fun I’ve had programming in a long time? \
> A: Yes.

There's really something to be said about this style of writing parsers.
To [paraphrase](https://news.ycombinator.com/item?id=41756917)
Marshall Lochbaum, the creator of BQN: it's
hard to write,
hard to read,
hard to modify,
but an absolute joy to debug,
and I would have to agree on all points.
Especially the debugging part, though:
since there's essentially no branching, control flow is trivial,
and furthermore everything is just a flat array of numbers.
Once that one `0` that should be a `1` is identified, going back to the source just involves reading backwards, with no logic to keep track of at all.

Interestingly, this feels quite different from ordinary array programming—<!--
-->especially the quite tricky task to give lots of names[^17] to lots of concepts.
Still, I suppose this is much more readable than if one tried to make the names go away by using combinators.
At least there are comments, right?

I certainly wouldn't write all—or even most—parsers in this style,
but honestly this whole thing was so refreshing that I can't help but recommend anyone to try at least getting the hang of a simplified form of one of the parsing libraries in [bqn-libs][bqn-libs].

[^1]: {-} 󠀠

      We could in fact also move *replicate*
      into the calls to `ns` and `𝕩`:

      ``` bqn
          '0'¨⌾((ts/ns)⊸/) (ts/inp)
      "[0,0,0,0,0]"
      ```

      I'm not super well-versed in the ins and outs of CBQN's performance,
      but from a cursory benchmark with the `•_timed` modifier,
      it does not seem to make much of a difference.

[^5]: {-} 󠀠

      You will see repeated definitions of `Tokenise` and `Parse` throughout this article.
      I figured that perhaps indicating changes in this way is more readable than giving you diffs,
      if only because all context one needs is right there.
      That's one of the advantages of the complete program fitting on a single screen!

[^6]: {-} 󠀠

      󠀠

      Don't worry too much about having to use `0∾on` instead of `on` here;
      this appears because we are starting at depth zero before reading anything.
      Assuming well-formed JSON, this will appear only once at the very end.

[^7]: With a simple extension to the tokeniser to recognise the constants `true`, `false`, and `null`,
      this already parses the `cargo metadata` output of a few Rust crates that I've tried,
      which is the sort of data I would characterise as "ordinary JSON".
      The resulting representation is probably not super easy to work with<!--
      -->—there's quite a lot of metadata and thus the nesting gets quite deep—<!--
      -->but still, it works.

[^8]: Although CBQN seems to have improved since then<!--
      -->—for me, even the nested array version is faster than the recursive approach now.

[^9]: If you feel like I'm going too slow, or that I got something wrong, sorry!
      As I said, this is more or less my way of learning and better understanding this style of parsing;
      I am by no means the expert here.

[^10]: {-} 󠀠

       I will present many functions in a bottom-up kind of way,
       where I gradually build them up in the REPL,
       using an example input to showcase the behaviour of the individual array manipulations.
       Usually, proper definitions of `Tokenise` or `Parse` (the two functions we're building)
       will only be given at the very end of each section.
       The input is always indented by four spaces, with the output flush to the left.
       If I refer to an unknown variable further down, it was probably defined in such a block.

[^11]: {-} 󠀠

       󠀠

       󠀠

       󠀠

       󠀠

       󠀠

       As you can see, an opening parenthesis is treated as being inside of the pair,
       while a closing one is outside.
       This is fine (even wanted) for this use-case;
       if you want to treat both pairs as being inside of the pair you can use
       ``+`o-»c``.

[^12]: A questionable API choice, for sure, but I'll take it!

[^13]: {-} The double quotes `""` in the string are BQN's way of escaping quotes
       (the same way that CSV does it).
       *Everything* else is treated as a literal character, even a backslash!

[^14]: I will use this as shorthand for "sub-object and sub-array".
       Not the best terminology, I suppose, but it will have to do.

[^15]: Remember that questionable *group* API?

[^16]: {-} 󠀠

       I find this process of treating values and keys separately,
       but in a way that they eventually match up at the end,
       almost magical.

[^17]: Short names at that, otherwise things just look too cumbersome in this style of programming.

[^18]: {-} In case anyone not familiar with BQN finds themselves reading this post: BQN is an array programming language in the APL-family.

[bqn-libs:json]: https://github.com/mlochbaum/bqn-libs/blob/master/json.bqn
[bqn-libs]: https://github.com/mlochbaum/bqn-libs
