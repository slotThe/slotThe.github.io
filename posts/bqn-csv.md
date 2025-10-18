---
title: Parsing CSV in BQN
date: 2025-07-28
tags: BQN
---

Let's write a CSV parser in [BQN](https://mlochbaum.github.io/BQN)!

<!--more-->

Working my way towards a full blown JSON parser,
I thought it might be a good idea to start with something simpler<!--
-->—[RFC 4180](https://www.ietf.org/rfc/rfc4180.txt) sounds like a good candidate.[^1]
Plus, there is also a great reference implementation available in
[bqn-libs](https://github.com/mlochbaum/bqn-libs/blob/master/csv.bqn).

This article will assume that you are already at least a little bit familiar with BQN as a language.
If not, I would recommend starting with the official [tutorials](https://mlochbaum.github.io/BQN/tutorial/index.html).
I've also written about my experience [using BQN](https://tony-zorman.com/posts/aoc-in-bqn.html) for Advent of Code,
which certainly contains some amount of written words that talk about the language.

# The simplest case

We'll start with a subset of CSV in which we don't have to think about escaping at all.
That is, we only allow files that look like this:

```
a,b,c,d
e,f,g,h
```

Let's also permit lines of varying lengths;
this doesn't really change the implementation much, but I find it quite convenient sometimes.
This simple case is quite straightforward to implement:
we start with finding all occurrences of newlines and commas in the input string

``` bqn
    inp
"a,b,c,d
e,f,g,h"
    [c,n] ← (','∾@+10) =⌜ inp
┌─
╵ 0 1 0 1 0 1 0 0 0 1 0 1 0 1 0
  0 0 0 0 0 0 0 1 0 0 0 0 0 0 0
                                ┘
```

and then split the string accordingly:

``` bqn
    s ← n∨c                          # Where to split
⟨ 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 ⟩
    (1-˜(¬s)×1+`s) ⊔ inp
⟨ "a" "b" "c" "d" "e" "f" "g" "h" ⟩
```

Having all of the "tokens" ready,
one just needs to split at the newlines while keeping the shorter list of all splits in mind.

``` bqn
    (0∾s/+`n) ⊔ (1-˜(¬s)×1+`s) ⊔ inp
⟨ ⟨ "a" "b" "c" "d" ⟩ ⟨ "e" "f" "g" "h" ⟩ ⟩
```

Easy.

Now, this was probably quite fast—if not, feel free to skip to the next section.
Let's go through that last line more carefully, as it contains lots of important concepts for array-oriented parsing.

First, we create a depth scan according to all characters we'd like to split the string at:[^2]

``` bqn
    inp≍ 1+`s
┌─
╵ 'a' ',' 'b' ',' 'c' ',' 'd' '\n' 'e' ',' 'f' ',' 'g' ',' 'h'
   1   2   2   3   3   4   4   5    5   6   6   7   7   8   8
                                                              ┘
```

To single out all of the characters that we'd like to delete<!--
-->—i.e., the ones we are splitting at—<!--
-->we multiply the depth scan by `¬s`,
set all commas and newlines to `0`,
and then subtract one.

``` bqn
    inp≍ 1-˜(¬s)×1+`s
┌─
╵ 'a' ',' 'b' ',' 'c' ',' 'd' '\n' 'e' ',' 'f' ',' 'g' ',' 'h'
   0  ¯1   1  ¯1   2  ¯1   3   ¯1   4  ¯1   5  ¯1   6  ¯1   7
                                                              ┘
    sl ← (1-˜(¬s)×1+`s) ⊔ inp
⟨ "a" "b" "c" "d" "e" "f" "g" "h" ⟩
```

This is also why we gave the depth scan above an initial argument of `1`—otherwise, we'd get

``` bqn
    inp≍ 1-˜(¬s)×+`s
┌─
╵ 'a' ',' 'b' ',' 'c' ',' 'd' '\n' 'e' ',' 'f' ',' 'g' ',' 'h'
  ¯1  ¯1   0  ¯1   1  ¯1   2   ¯1   3  ¯1   4  ¯1   5  ¯1   6
                                                              ┘
```

Next on the agenda is splitting the split list `sl` itself according to line breaks.
For that we again do a depth scan, only this time we use the newline array `n`.

``` bqn
    +`n
⟨ 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 ⟩
```

Now we replicate the appropriate cells in this array according to `s`.
This yields a grouping of the different splitting characters into their respective lines.

``` bqn
    inp≍+`n
┌─
╵ 'a' ',' 'b' ',' 'c' ',' 'd' '\n' 'e' ',' 'f' ',' 'g' ',' 'h'
   0   0   0   0   0   0   0    1   1   1   1   1   1   1   1
                                                              ┘
    (s/inp)≍(s/+`n)
┌─
╵ ',' ',' ',' '\n' ',' ',' ','
   0   0   0    1   1   1   1
                              ┘
```

Since we don't operate on the objects directly,
but rather on their "connections"—I think of this a bit like a graph—,
we additionally have to add a starting cell for the first object.

``` bqn
    (0∾s/+`n) ⊔ sl
⟨ ⟨ "a" "b" "c" "d" ⟩ ⟨ "e" "f" "g" "h" ⟩ ⟩
```

Putting it all together, we have something like

``` bqn
CSV ← {
  [c,n] ← (','∾@+10) =⌜ 𝕩
  s ← n∨c
  (0∾s/+`n) ⊔ (1-˜(¬s)×1+`s) ⊔ 𝕩
}
```

which works as expected:[^5]

``` bqn
    CSV "aaaa,b,c,d"∾(@+10)∾"e,f"
⟨ ⟨ "aaaa" "b" "c" "d" ⟩ ⟨ "e" "f" ⟩ ⟩
```

# Adding escaping

If a field contains commas or newlines, they need to be escaped in some way.
The way that CSV handles this is that those fields are to be enclosed in double quotes:

```
a,"b,c
d,e"
```

This is a single line with two fields, one containing `a` and one containing `b,c,\nd,e`.
Within a quoted field, a `"` can be written as `""`;
the following example consists of a single field with value `"a","b"`

```
"""a"",""b"""
```

In terms of parsing,
the first thing we need is to somehow figure out which characters are escaped.
Thankfully, this turns out to be pretty easy—start in an unescaped setting,
and then every time a quote is encountered, flip some "are we quoted yet?"-bit.
In code, this can rather beautifully be expressed as a not-equal scan:[^4]

``` bqn
    inp ← "a,""b,""""b'"""",b''"",c"    # a,"b,""b"",b''",c
"a,""b,""""b'"""",b''"",c"
    [q,c,n] ← ('"'∾','∾@+10) =⌜ inp
┌─
╵ 0 0 1 0 0 1 1 0 0 1 1 0 0 0 0 1 0 0   # quotes   q
  0 1 0 0 1 0 0 0 0 0 0 1 0 0 0 0 1 0   # commas   c
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0   # newlines n
                                      ┘
    e ← ≠`q                             # escaped
⟨ 0 0 1 1 1 0 1 1 1 0 1 1 1 1 1 0 0 0 ⟩
    inp≍e
┌─
╵ 'a' ',' '"' 'b' ',' '"' '"' 'b' ''' '"' '"' ',' 'b' ''' ''' '"' ',' 'c'
   0   0   1   1   1   0   1   1   1   0   1   1   1   1   1   0   0   0
                                                                         ┘
```

The next step is a little non obvious—at least to me.
Instead of having a single variable `s←n∨c` that controls where to split the text,
we will instead have two: one for where to split, and one for which separators to drop.
The latter is needed because we don't actually want to keep all of the double quotes when parsing the CSV into a BQN type.
For example, the above `"a,""b,""""b'"""",b''"",c"` should parse to something like `⟨"a"‿"b,""b'"",b''"‿"c"⟩`.

For splitting, we can use the exact same logic as in the first version of the parser,
now additionally taking into consideration the escaped flag.
Using the definitions from above:

``` bqn
    inp≍ s←(¬e)∧c∨n  # split
┌─
╵ 'a' ',' '"' 'b' ',' '"' '"' 'b' ''' '"' '"' ',' 'b' ''' ''' '"' ',' 'c'
   0   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   1   0
                                                                         ┘
```

Dropping is a little bit more finicky,
so let's start with something slightly simpler.
Essentially, we want to drop double quotes that are used to start an escaped field,
and only want to keep one pair of quotes on the inside of such a field.
The above mentioned transformation

```
"a,""b,""""b'"""",b''"",c"   ->   ⟨"a"‿"b,""b'"",b''"‿"c"⟩
```

should illustrate what I mean.
One way to achieve this is to mark all last occurrences of double quotes,
along with all separators.

``` bqn
    inp≍ d←s∨«⊸<q # what to drop
┌─
╵ 'a' ',' '"' 'b' ',' '"' '"' 'b' ''' '"' '"' ',' 'b' ''' ''' '"' ',' 'c'
   0   1   1   0   0   0   1   0   0   0   1   0   0   0   0   1   1   0
                                                                          ┘
```

Cutting up the input string now works essentially like the first version,
just that we have to take care where to insert `s` and where to insert `d` now.
Only "real" separators should increase the depth of an expression,
while for singling them out we have to consider all characters that are to be dropped.

``` bqn
    1-˜(¬d)×1+`s
⟨ 0 ¯1 ¯1 1 1 1 ¯1 1 1 1 ¯1 1 1 1 1 ¯1 ¯1 2 ⟩
    (1-˜(¬d)×1+`s) ⊔ inp
⟨ "a" "b,""b'"",b''" "c" ⟩
```

Likewise, when grouping the fields by line we have to be careful to only talk about unescaped newlines.

``` bqn
    +`(¬e)∧n
⟨ 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ⟩
    0∾s/+`(¬e)∧n
⟨ 0 0 0 ⟩
    (0∾s/+`(¬e)∧n) ⊔ (1-˜(¬d)×1+`s) ⊔ inp
⟨ ⟨ "a" "b,""b'"",b''" "c" ⟩ ⟩

```

All in all, the second version of our CSV function looks like this:

``` bqn
CSV ⇐ {
  [q,c,n] ← ('"'∾','∾@+10) =⌜ 𝕩        # Quote, comma, LF
  e ← ≠`q                              # Escapes
  s ← (¬e)∧n∨c                         # Where to split
  d ← s∨«⊸<q                           # What to drop
  (0∾s/+`(¬e)∧n) ⊔ (1-˜(¬d)×1+`s) ⊔ 𝕩  # First split all, then split lines
}
```

---

This works in almost all cases already,
though there is some unexpected behaviour surrounding empty fields:

``` bqn
    CSV "a,,b"
┌─
· ⟨ "a" ⟨⟩ "b" ⟩
                 ┘
    CSV "a,"""",b"
⟨ ⟨ "a" """" "b" ⟩ ⟩
```

Those two expressions should probably be equivalent.
Since we already mark the last occurrences of each quote,
we just have to make sure that we additionally mark those that are also escaped.
Changing

``` bqn
  d ← s∨«⊸<q                           # What to drop
```

to

``` bqn
  d ← s∨q∧e∨«⊸<q                       # What to drop
```

now parses both cases correctly:

``` bqn
    (CSV "a,,b") ≡ CSV "a,"""",b"
1
```

---

Here are some exercises for the interested reader:

1. Generalise the `CSV` function to take arbitrary separators instead of just a comma.
   This can rather elegantly be achieved by making it an ambivalent function,
   with the monadic case just being `',' CSV 𝕩`.

2. Add some failure states, like actually honouring

   > Each line should contain the same number of fields throughout the file.

   from the RFC.
   This could just use
   [assert](https://mlochbaum.github.io/BQN/doc/assert.html)
   to print an error message to the user.

3. I've very efficiently ignored that CRLF line endings exist, so I guess that one's still missing?

# Conclusion

Q: Is writing (and reading) a parser in an array language unnecessarily hard? \
A: Yes.

Q: Is it the most fun I've had programming in a long time? \
A: Yes.

[^1]: For the sake of convenience, I will interpret the RFC somewhat loosely in places,
      though hopefully still implementing the gist of it.

[^2]: {-} BQN doesn't actually escape non-double-quote characters in strings,
      but I've chosen to write `\n` here instead of a literal newline for readability.

      Also, I will use
      [couple](https://mlochbaum.github.io/BQN/doc/couple.html)
      quite a bit to visualise the different masks in relation to the original character array going in,
      in case you were wondering what that glyph meant.

[^4]: {-} 󠀠

      As you can see, escaping is… fun.
      Every character save a double quote is verbatim in BQN strings,
      and double quotes are also escaped using two of them in succession.
      This means that to input the CSV `"a"`, one would write `"""a"""`,
      and for `"""a"",""b"""` we have the wonderful
      `"""""""a"""",""""b"""""""`.

[^5]: {-} 󠀠

      Since BQN doesn't do escaping in strings,
      I make use of
      [character arithmetic](https://mlochbaum.github.io/BQN/doc/types.html#characters)
      here, to represent the line feed by its decimal value.
