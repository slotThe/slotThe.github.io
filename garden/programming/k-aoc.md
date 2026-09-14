---
title: Advent of Code in K
date: 2026-09-12
tags: array-lang, k
tended: 2026-09-14
---

Let's do some k.

<!--more-->

They say the best way to learn a language is to write "useful" programs in it. Screw that, I say, let's do some fun puzzles instead! I hope[^1] this will not be another [20'000 word post](https://tony-zorman.com/posts/aoc-in-bqn.html), but you never know. Seriously, though, I will try to be a bit more terse—get it?—than in previous posts, if only to save my sanity.


# Introduction


## Setup

I'll just quote the BQN post here, as this part is sort of the same:

> Advent of Code is a yearly puzzle-coding-challenge-thing, in which one has to solve two riddles every day from the 1st until the 25th of December, with the second part of each puzzle being unknown until one solves the first one. I’ve found it to be a great way to learn the basics of a language—how it “feels”.
>
> The puzzles are accompanied by an endearing—and at times pretty funny—story. I will, however, leave out most of it for this post, as I feel like it would distract too much from the point I’m trying to make.


## Disclaimer

I don't actually know this language. Much like the BQN post, this post is mostly aimed at myself—writing things down is a great way to learn them, it turns out. As a result, expect many little inaccuracies and probably also some more major oopsies.[^2] If you actually want a good introduction to the language, I recommend [razetime's tutorial](https://razetime.github.io/ngn-k-tutorial/) and the [kparc crash course](https://github.com/kparc/kcc).


## Conventions

This article is written in a literate style; see [this post](file:///posts/interactive.html) for more information. Right now, this just means that whenever you see a code block defining a variable or function

```k
aliteralone:1; Add:+;
two:((1 1); (2 3))
```

then expect it to carry over to the next code block. Additionally, most examples will be presented in a REPL-esque fashion; input is indented by a space, and output is flush to the left. Line comments start with ~ /~.[^3]

```k
 Add[aliteralone;aliteralone] / M-Expressions can be used for function application
2
```

Think of each day as introducing its own namespace, though; things would get out of hand fast otherwise, considering the tersity of most names!

Additionally, for better search (`Full solution:`), I will include the full solution at the end of each day.


# A crash course on syntax

Here is an entirely too short crash course on the syntax of k. If you've read the [BQN version](https://tony-zorman.com/posts/aoc-in-bqn.html#a-crash-course-on-syntax) of this, a lot of it will feel familiar, although sometimes in an uncanny valley type of way.

```k
 1+4+6 / 1+(4+6)
11
 2*4+6 / 2*(4+6)
20
```

As with most (all?) array languages, builtin functions—​*verbs*, in k parlance—have the same precedence, and expressions are evaluated strictly right to left.

Assignment uses `:`​—one of its many, many uses—and multiple expressions on one line are separated by `;`:[^4]

```k
 :a:2*4+6 / Another use for : is "right"
20
 b:1+1    / By default, you won't see the result of an assignment
 a
20
```


### Lists and atoms

The fundamental data structure in k is the list. Some homogeneous lists (e.g., numbers or symbols) can be created just by juxtaposition; general lists use parentheses and semicolons. Strings are lists of characters.

```k
 1 2 3
1 2 3
 (1;2;3)
1 2 3
 ("hi";42;1 2 3)
("hi"
 42
 1 2 3)
 "a" "b" / Error!
'type
 "a" "b" / Error!
     ^
 ("a";"b")
"ab"
 `a`b    / This is ok
(`"a"
 `"b")
```

A symbol in K starts with a backquote, followed by some string of ASCII characters. You can think of `` `a `` as an analogue of `'a` in a Lisp. In general, K seems to be inspired quite a bit by that family of languages, as we'll see when we get to functions and indexing.

Unlike many other array languages, k does not have "proper" multidimensional arrays; instead, they're represented by nested lists.

```k
 3 3#!9 / ! is iota and # is reshape
(0 1 2
 3 4 5
 6 7 8)
```

This also means that these "arrays" can be ragged without any troubles.

```k
 (0 1;2 3 4;5)
(0 1
 2 3 4
 5)
```

Indenting things by one additional space in the REPL turns off the kind of pretty-printing,[^5] though you can often also just *enclose* (`,`) the result.

```k
 ,3 3#!9
,(0 1 2;3 4 5;6 7 8)
```


### Nouns, verbs, and adverbs

Roughly, instead of variables, operators, and higher-order functions, K instead talks about *nouns*, *verbs*, and *adverbs*.

Nouns are.

---

Verbs act on nouns. More specifically, they're builtin functions, and more or less equivalent to what an operator would be in other languages—that is, they are (or, rather, can be) written using infix notation, and take one (to the right) or two (to the left and right) arguments. For example, the symbol `%` is a square root function when supplied with one argument, and floating point division in the two argument case:

```k
 %8     / Square root of 8
2.8284271247461903
 %4 6   / Square root of 4 and of 6
2.0 2.449489742783178
 1%2    / ½
0.5
```

The fact that `%` transparently works on an array is called *scalar pervasion*, and is one of the backbones of a lot of array languages.[^6]

Instead of these verbs having a "unary" and "binary" case, we instead say they are *monadic* or *dyadic*. Greek is better than Latin, or something. In K, this is actually a little bit more complicated, as the mere presence of one or two arguments is not enough to disambiguate a function: you also need to know about the types of the arguments. More on that later.

---

Adverbs modify verbs; you can think of them as special notation for higher-order functions, from a time when perhaps this wasn't a thing people thought about much.

For example, *each*, `f'`​, maps `f` over `x`; this might be written `map f x` in Haskell, or `x.map(f)` in Rust.

```k
 {x+1}'1 2 3 4 / We'll talk about user-defined functions soon
2 3 4 5
```

Two of the most useful adverbs are *fold* (`F/`) and *scan* (`F\`), both of which are the left variants of these respective operations.

```k
 +/!9      / Sum up all numbers from 0 to 8
36
 -/!4      / Left fold: ((0-1)-2)-3
-6
 {y-x}/|!4 / Emulating a right fold using reverse |x
-2
 +\!9      / Scan shows intermediate results
0 1 3 6 10 15 21 28 36
```

---

There are no precedence rules for verbs in K, but adverbs still bind tighter than verbs, since they need one of the latter to be correctly specified. That is, something like `,//` is grouped like `(,/)/`.


### Lambdas

You've already seen some user-defined functions, like `{x+1}` or `{y-x}` above: they're written with curly braces, and each function can have up to three implicit arguments that you can use: `x`, `y`, and `z`. These are very much like `α` and `ω` in APL, or `𝕨` and `𝕩` in BQN.

```k
 {x+1} 5
6
 {x+y}[3;4]
7
 dbl:{x+x}; dbl 5
10
```

User-defined functions are automatically nouns instead of verbs, so they're treated like variables, instead of the builtin functions.[^7] This has a few implications: first, they can't be used infix, and one instead has to resort to one of the many types of function application syntaxes, some of which we've already seen above (and one of which is M-Expression syntax!)

```k
 Add:{x+y}
 Add[2;3]  / M-Expression
5
 Add[2]    / Functions are automatically curried!
{x+y}[2;]
 Add[2][3]
5
 Add[2] 3  / Juxtaposition also works
5
 Add[2]@3  / Another verb for application
5
 Add.(2;3) / TODO
5
```

Some of these are necessary in one situation or another. For example, monadic `%` gives the square root of a number, so `Add[2] %5` should yield $2 + \sqrt{5}$, but instead yields a syntax error. The `Add[2]`, being a noun, is treated as another argument to `%` and it tries to do its dyadic overload, *divide*. To fix this one has to use `@`.

```k
 Add[2] %5 /error
'type
 Add[2] %5 /error
        ^
 Add[2]@%5 /ok
4.23606797749979
```

---

Indexing into an array uses the same syntax as function application:

```k
 :a:3 3#!9
(0 1 2
 3 4 5
 6 7 8)
 a[0;1]  / Single element
1
 a[2]    / Row
6 7 8
 a[0][1] / Single element
1
 a[0] 1  / Juxtaposition also works
1
 a[0]@1  / Another verb for application
1
 a.(0;1) / TODO
1
```

There's again some different behaviour when it comes to `@` and `.`:

```k
 :a:3 3#!9
(0 1 2
 3 4 5
 6 7 8)
 a@0 1    / First and second row
(0 1 2
 3 4 5)
 a.0 1    / Element at index 0 1
1
```


### Type-based overloading

Verbs in K are overloaded quite a bit; for example, `!` has as many as seven (7) meanings, depending on the types of its parameters:

```k
 !10       /   !i is enum
0 1 2 3 4 5 6 7 8 9
 !1 2 3    /   !I is odometer
(0 0 0 0 0 0
 0 0 0 1 1 1
 0 1 2 0 1 2)
 `a`b!1 2  /  x!y is dict
!/+((`"a";1)
    (`"b";2))
 !`a`b!1 2 /   !d is keys
(`"a"
 `"b")
 3!10 12   /  i!I is mod
1 0
 -3!10 12  / -i!I is div (really!)
3 4
 / … and so on
```

Needless to say, this turns reading K into quite an adventure. To make this slightly easier, I'll do what the REPL does, and indicate the types by a single letter; instead of "dyadic `!`", I'll talk about `i!I` instead. Here's the relevant table:

```
notation: [c]har [i]nt [n]umber(int|float|char) [s]ymbol [a]tom [d]ict
          [f]unc(monad) [F]unc(dyad) [xyz]any
```

Upper case letters indicate a list of the respective type. These are (almost) the same letters that K gives you when you ask it the type of things with `@x`:

```k
 @1
`i
 @1 2 3
`I
 @`a!1  / We'll talk about dicts at some point, I promise
`m
 @"a"   / A single characters in ""s is automatically a char
`c
 @"abc"
`C
```


# Days 1–5


## [Day 1](https://github.com/slotThe/advent/blob/master/aoc2020/k-solutions/day01.k)

We're going on vacation! However, of course we'll first have to do some work, in order to earn the exotic local currency used on the island we'd like to travel to.

Today, the elves want us to help them with their expense records; for part one, we have to find the two entries in the input that sum up to 2020 and multiply them together. The file is just a big list of integers, separated by newlines; one of the many uses of the colon in K is for reading a file line by line with `0:`.

```k
 10# 0:"./aoc/2020/day01.txt"  / take (i#y) the first 10 lines, for display reasons
("1895"
 "1732"
 "1660"
 "1658"
 "1878"
 "367"
 "2010"
 "1989"
 "431"
 "1946")
 `I$ 0:"./aoc/2020/day01.txt"  / s$y is cast, and `I means a list of integers
1895 1732 1660 1658 1878 367 2010 1989 431 1946 1614 2003 945 1856 1934 1937 1781 1947 1991 1917 1604 1707 1966 1959 1182 1828 1880 1908 1942 1687 1611 1922 1913 1803 1976 1718 1885 1971 2000 1912 1981 1776 1901 1941 1935 1977 1907 1893 1898 1975 2001 1833 1951 1939 19..
 i:`I$0:"./aoc/2020/day01.txt" / call this list i
 #i                            / #x is length
200
```

The file is only 200 entries long, so we don't have to do anything fancy here, and a simple outer product should be enough: compare each number with all other numbers, and if the sum is 2020 we're good. K has *each left* (`x F\: y`) and *each right* (`x F/: y`) for purposes like this. For example, the latter would apply `F` to `a` and `y`, for all elements `a` of `x`.

```k
 :x:!4       / !i is iota/range
0 1 2 3
 :y:!5
0 1 2 3 4
 x +\: y     / (0 + y; 1 + y; and so on)
(0 1 2 3 4
 1 2 3 4 5
 2 3 4 5 6
 3 4 5 6 7)
```

So, the strategy is: form an outer product of `i` with itself under addition, compare each entry with 2020, get a mask of all hits, get the first hit, use these values to index into `i`, and multiply the numbers together. Getting the indices of hits can be done with *where* (`&I`); everything else just uses operations we've seen already, stuck together in new and creative ways.

```k
 &1 0 1 0 1 0              / Only give me the indices that are 1
0 2 4
 & 2020= i+\:i             / indices of hits
(110 174
 174 110)
 {i@x}'& 2020= i+\:i       / indexed back into i
(1191 829
 829 1191)
 **/{i@x}' & 2020= i+\:i   / and multiplied (and the first of those)
987339
```

---

For part two, we instead need to find the three numbers that sum up to 2020. As we are still only considering a list of length 200, this doesn't need any new algorithmic insight; we can just repeat what we've already done, using an iterative *each left*.

```k
 **/{i@x}' & 2020= i+\:i+\:i
259521570
```

We could have also written this without the curly braces:

```k
 **/(i@)' & 2020= i+\:i+\:i
259521570
```

This showcases an important feature of K, which I'll make use of quite a bit going forward: trains!

<details><summary>Trains in K</summary>

A train consists of a sequence of functions and arguments (verbs and nouns), that is however being terminated by a verb. Normally, this would not have any actual meaning as an expression, as it's not given an argument (noun) to act upon. However, interpreting this as a kind of function composition, we can use this to glue together verbs into something bigger, without naming arguments at all. This is usually called *tacit programming*.[^8]

For example, `A:{x+y}` could just be written as `A:+`. Since verbs in K have both a monadic and a dyadic meaning, the way in which one uses them in trains must be unambiguous. For that, yet another overload of the colon is used—a verb followed by a colon indicates that it's monadic, so while `A:+` signifies that the `A` function adds two arrays, `T:+:` instead defines a function `T` for transposition.

Here's a slightly more complicated example: given two $n \times n$-matrices $A = (a_{ij})$ and $B = (b_{ij})$, their product at the index $(i,j)$ is defined by $$ (A B)_{ij} = \sum_{k=1}^{n} a_{ik}b_{kj}. $$

Let's write a K function to compute the matrix multiplication of two square matrices.[^9]

```k
 Mn:{+/'x*\:y}   / Matrix multiplication, "normal"
 Mt:+/'*\:       / Matrix multiplication, tacit
 :A:3 3#!9
(0 1 2
 3 4 5
 6 7 8)
 :B:3 3#5+!9
(5 6 7
 8 9 10
 11 12 13)
 Mn[A;B]
(30 33 36
 102 114 126
 174 195 216)
 Mn[A;B]~Mt[A;B] / Dyadic ~ is "matches?"
1
```

Why would anyone ever program like this? Well, besides the fact that it's fun, it's just another case of really internalising the "names don't matter" mantra. Since the implementation `+/'*\:` has exactly as many characters as the name `matmul`, perhaps there is something to that line of thinking.

</details>

---

Full solution:

```k
i:`I$0:"./aoc/2020/day01.txt"
987339   = **/(i@)'&2020=i+\:i
259521570= **/(i@)'&2020=i+\:i+\:i
```


## [Day 2](https://github.com/slotThe/advent/blob/master/aoc2020/k-solutions/day02.k)

Today we have to help a shopkeeper fix the computer system of the Toboggan Rental Shop at the airport. Of course, to ensure the safety of the system, all passwords must follow a strict policy. Part 1 asks us how many valid passwords there are in the bunch.

```k
 0:"./aoc/2020/day02.txt"
("3-4 j: tjjj"
 "7-10 h: nhhhhhgghphhh"
 "7-13 j: tpscbbstbdjsjbtcpj"
 "4-13 l: ckllmqzlvcsxpplqg"
 "3-11 n: nnrhnnnnnnnwsdnnnm"
 "5-6 d: ddddddb"
 "7-10 z: szfwzrbzzz"
 "1-10 w: wwwwwcwwwrpnwzwxww"
 "5-6 w: cgwppfwlwrwtnw"
 "12-13 m: bmmthmmhmmmbmmmmm"
 "1-4 b: gbbj"
 "10-14 k: kkkkkkkkkfkkkzk"
 "1-3 n: nndn"
 "3-4 w: wwpf"
 "15-16 q: qqqqqqqqqqqqrqqqq"
 "3-4 w: wwcg"
 "10-12 r: rrrrrrrrrcrrrr"
 "10-11 v: fvlvrvcxvvp"
 "4-11 l: qdklwltggdcqn"
 "2-4 p: ppskptpp"
 "3-8 r: rjrrgrrk"
 ..)
```

Each line indicates the lowest and highest number of times the given letter may occur in the password; so `3-4 j: tjjj` would say that `j` must occur three, and may at most occur four, times in the string `tjjj`. Let's do some parsing first.

```k
 :test: *" "\' 0:"./aoc/2020/day02.txt" / C\ is split
("3-4"
 "j:"
 "tjjj")
 {(.'"-"\x 0;*-1_x 1;x 2)} test
(3 4
 "j"
 "tjjj")
 :(f;c;s): + {(`I$"-"\x 0;*-1_x 1;x 2)}' " "\' 0:"./aoc/2020/day02.txt"
((3 4;7 10;7 13;4 13;3 11;5 6;7 10;1 10;5 6;12 13;1 4;10 14;1 3;3 4;15 16;3 4;10 12;10 11;4 11;2 4;3 8;5 9;9 16;4 5;2 11;4 7;2 4;8 9;2 9;3 4;16 18;2 12;8 9;1 3;2 10;8 11;3 14;2 3;1 6;1 10;2 11;10 15;6 7;17 18;7 8;3 4;3 10;7 16;2 4;1 3;7 8;14 15;7 19;1 6;2 6;11 15;6 14;..
 "jhjlndzwwmbknwqwrvlprgnvrqsmxdqsqdhlwmbcgtsjmmhxbjmpdlfsscncblhdxxfbjstpjblgftpwgxddxptwnvvdbgxrzxdgpfrwkgxzgnkmwqrbdmqnlmbwlzhbqgdhhxnzfpgvrgxhsrmlfdbknstkxddtsdnspshzjqwqksgqgnknjnffghbxcrgfjdbsdxtmlhdjqbjtzxbwwskfznpbkxkkpbbggsmbllhhrpfnmgmcjrjhpqtrwnbvwwrffrxrdxh..
 ("tjjj";"nhhhhhgghphhh";"tpscbbstbdjsjbtcpj";"ckllmqzlvcsxpplqg";"nnrhnnnnnnnwsdnnnm";"ddddddb";"szfwzrbzzz";"wwwwwcwwwrpnwzwxww";"cgwppfwlwrwtnw";"bmmthmmhmmmbmmmmm";"gbbj";"kkkkkkkkkfkkkzk";"nndn";"wwpf";"qqqqqqqqqqqqrqqqq";"wwcg";"rrrrrrrrrcrrrr";"fvlvrvcxvvp";"qd..)
```

After splitting each line on spaces, we apply the function ``{(`I$"-"\x 0;-1_x 1;x 2)}`` to each element: it splits the first element, which looks like `15-34`, on the dash, and then again *casts* the resulting list of strings into a list of integers. For the second entry, `j:`, we *drop* the last element by giving `i_X` a negative value as a left argument.

At the very end, we *transpose* the input, so that instead of a list of three-element lists, we get three lists of the individual things to consider: `[f]requency`, `[c]har`, and `[s]tring`. This kind of transposition is often used in array languages, as now one can really make use of primitives working on arrays.

Checking whether a password is valid involves checking whether all occurrences of a given char in the associated string lies in the frequency interval. Comparing a char with its string is easy:

```k
 5# c=s   / All comparisons at once
(0 1 1 1
 0 1 1 1 1 1 0 0 1 0 1 1 1
 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 1
 0 0 1 1 0 0 0 1 0 0 0 0 0 0 1 0 0
 1 1 0 0 1 1 1 1 1 1 1 0 0 0 1 1 1 0)
 +/' c=s  / Number of occurrences
3 9 3 4 12 6 5 12 5 12 2 13 3 2 16 2 13 5 2 5 5 14 16 2 2 2 12 8 14 4 16 14 4 9 15 3 17 1 10 5 8 2 7 14 4 3 3 3 3 4 6 14 10 2 10 11 13 3 8 5 4 9 5 10 6 10 11 5 2 11 1 14 3 7 3 6 9 4 5 6 11 12 1 5 8 8 2 19 4 2 11 5 5 0 3 10 14 2 10 1 8 3 2 9 15 3 4 3 2 6 2 9 3 18 2 5 11..
```

All that's left is to check the interval condition. Now, I have to say, this brings us to a bit of an annoying corner of K, at least in my opinion: the lack of proper `<=` and `>=` operators. They just don't exist, so we'll have to do some logical transformations.[^10] Notice, for example, that $$ x \in [a,b] \iff a \le x \land x \le b \iff x<a \oplus x<b+1 $$ Yes, this is a horribly roundabout way of expressing that particular fact, but it can at least be implemented in K straightforwardly:

```k
 5# (0 1+)'f                / Turn b into b+1
(3 5
 7 11
 7 14
 4 14
 3 12)
 5# (+/'c=s)<(0 1+)'f       / Comparison
(0 1
 0 1
 1 1
 0 1
 0 0)
 5# 1=+/' (+/'c=s)<(0 1+)'f / XOR test
1 1 0 1 0
 +/ 1=+/' (+/'c=s)<(0 1+)'f / Answer for part one
483
```

---

The shopkeeper helpfully informs us that we've actually followed the wrong policy! Instead of the scheme so far, the numbers in `f` indicate but two 1-indexed positions in the string, and exactly one of those positions must contain the given character. Not so bad, we just have to slightly adjust the function for part 1.

```k
 5# f-1           / 0-indexed
(2 3
 6 9
 6 12
 3 12
 2 10)
 5# s@'f-1        / Getting the letters
("jj"
 "gp"
 "sj"
 "lp"
 "rn")
 5# 1=+/'c=s@'f-1 / Exactly one
0 0 1 1 1
 +/ 1=+/'c=s@'f-1 / Answer for part two
482
```

---

Full solution:

```k
(f;c;s): + {(`I$"-"\x 0;*-1_x 1;x 2)}' " "\' 0:"./aoc/2020/day02.txt" /freq, char, string
483= +/1=+/'(+/'c=s)<(0 1+)'f /x ∈ [a,b] ⇔ x≤b∧x≥a ⇔ x<a ⊕ x<b+1
482= +/1=+/'c=s@'f-1
```


## [Day 3](https://github.com/slotThe/advent/blob/master/aoc2020/k-solutions/day03.k)

Today, we'll have to safely manoeuvre a toboggan through a forest. As an input, we get a map of the area, with the trees denoted by hashes:

```k
 0:"./aoc/2020/day03.txt"
("...#...#..#....#..#...#..##..#."
 ".#..#.....#.#............###..."
 ".#...###....#.............##..#"
 "...##...##....#.....##..#.##..."
 ".....###.#.###..##.#.##.......#"
 "#...##.....#..........#..#.#.#."
 "......##.......##..#....#.#...."
 "....#.###.##..#.#..##.##....#.#"
 ".......#.......###.#.#.##.....#"
 ".........#.#....#..........#.#."
 ".#...##.....##.........#..#...."
 ".##....#.#.#...##......#......."
 "##.#.#..#....#....#....#...#.#."
 "##....#.#..##......#....##...#."
 "....#..#..##..#.###.......#.#.."
 ".....##....###...........#.#.##"
 "#.....##.........#....##......#"
 "........###.#..#....#....#....."
 "...#.......#.##..#.###......#.."
 "...............#..#....#.##...."
 "..#..###..#.#..#.........##..#."
 ..)
```

We're also helpfully informed that the pattern loops when going to the right, but not when going down.

We start at the upper left corner, and always move three squares to the right and one square down. Our task for part 1 is to find out how many trees we would hit this way.

For this, we can use another overload for `/` and `\`: `i f/` is *n-do* and `i f\` is *n-dos*. The former is perhaps easier to explain: it's just a for-loop, in which you don't have access to the loop variable. For example, `5 (1+)/ 3` is equivalent to

```
s = 3;
for _ in [0…5]: # inclusive!
  s += 1;
s                # 8
```

```k
 5 (1+)/ 3
8
```

Some lisps call this `dotimes`. The `\`-variant just keeps track of all intermediate results.

```k
 5 (1+)\ 3
3 4 5 6 7 8
```

We can use this *n-dos* to generate all positions: since we're going down one square in each step, just add `1 3` to the initial position of `0 0` that many times, wrapping around horizontally if necessary.

```k
 5# i:"#"=0:"./aoc/2020/day03.txt"     / #=1
(0 0 0 1 0 0 0 1 0 0 1 0 0 0 0 1 0 0 1 0 0 0 1 0 0 1 1 0 0 1 0
 0 1 0 0 1 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0
 0 1 0 0 0 1 1 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1
 0 0 0 1 1 0 0 0 1 1 0 0 0 0 1 0 0 0 0 0 1 1 0 0 1 0 1 1 0 0 0
 0 0 0 0 0 1 1 1 0 1 0 1 1 1 0 0 1 1 0 1 0 1 1 0 0 0 0 0 0 0 1)
 r:#i; c:#*i                           / Rows and columns
 ,(r-1){(0;c)!'1 3+x}\(0;0)            / Indices of points
,(0 0;1 3;2 6;3 9;4 12;5 15;6 18;7 21;8 24;9 27;10 30;11 2;12 5;13 8;14 11;15 14;16 17;17 20;18 23;19 26;20 29;21 1;22 4;23 7;24 10;25 13;26 16;27 19;28 22;29 25;30 28;31 0;32 3;33 6;34 9;35 12;36 15;37 18;38 21;39 24;40 27;41 30;42 2;43 5;44 8;45 11;46 14;47 17;48 20;..
 +/ (i.)' (r-1)((0;c)!'1 3+)\(0;0)     / Part one
187
```

Two interesting things about this: first, we have to use *apply* of the form `x.y` instead of `x@y`, because we don't want two rows at different locations, but a single element.

```k
 i@0 2    / first and third row
(0 0 0 1 0 0 0 1 0 0 1 0 0 0 0 1 0 0 1 0 0 0 1 0 0 1 1 0 0 1 0
 0 1 0 0 0 1 1 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1)
 i.0 2    / element at index 0 2
0
```

Second, while `i!I` is *mod*, we actually have an array on the left, which turns this into *dict* instead! To do what we want, we have to apply mod component-wise with *each*. Actually, maybe three interesting things: K is one of the few languages where `0!x` actually returns `x` itself! This is more consistent, mathematically, but a lot of languages forego that for, I suppose, ease of implementation.

---

Part two gives us some additional slopes to consider:

> -   Right 1, down 1.
> -   Right 3, down 1. (This is the slope you already checked.)
> -   Right 5, down 1.
> -   Right 7, down 1.
> -   Right 1, down 2.

Let's start by turning that part of our part one solution into a function.

```k
 whee:{+/ (i.)' ((-*x)!r-1)((0;c)!'x+)\(0;0)} / x is a slope of the form (down;right)
 */whee' (1 1; 1 3; 1 5; 1 7; 2 1)            / Part two
4723283400
```

This uses a very non-obvious overload of `!`: `(-i)!I` is not modding with a negative number, but actually integer division of (all of) `I` by `i`! No, I don't know why.

---

Full solution:

```k
i:"#"=0:"./aoc/2020/day03.txt"; r:#i; c:#*i
whee:{+/(i.)'((-*x)!r-1)((0;c)!'x+)\(0;0)} / x is a slope of the form (down;right)
187       = one:whee 1 3
4723283400= one**/whee'(1 1;1 5;1 7;2 1)
```


## [Day 4](https://github.com/slotThe/advent/blob/master/aoc2020/k-solutions/day04.k)

Today we are standing in an airport queue, and are tasked to manipulate the passport validity check. Each passport is expected to have the following fields:

-   `byr` (Birth Year)
-   `iyr` (Issue Year)
-   `eyr` (Expiration Year)
-   `hgt` (Height)
-   `hcl` (Hair Color)
-   `ecl` (Eye Color)
-   `pid` (Passport ID)
-   `cid` (Country ID)

Only passports that either have all of the fields, or are just missing the `cid` field should be allowed. Parsing is always *so much fun* in array languages, so let's get on with it.

```k
 ,0:"./aoc/2020/day04.txt" / easier to see
,("hgt:176cm";"iyr:2013";"hcl:#fffffd ecl:amb";"byr:2000";"eyr:2034";"cid:89 pid:934693255";"";"hcl:#b5c3db ecl:grn hgt:155cm pid:#baec97 iyr:2017";"byr:1939";"eyr:2020";"";"pid:526669252 eyr:1972";"hgt:152cm ecl:dne byr:1960 hcl:z iyr:2023";"";"eyr:2028 hcl:#c0946f hg..
 1:"./aoc/2020/day04.txt"  / easier to work with
"hgt:176cm\niyr:2013\nhcl:#fffffd ecl:amb\nbyr:2000\neyr:2034\ncid:89 pid:934693255\n\nhcl:#b5c3db ecl:grn hgt:155cm pid:#baec97 iyr:2017\nbyr:1939\neyr:2020\n\npid:526669252 eyr:1972\nhgt:152cm ecl:dne byr:1960 hcl:z iyr:2023\n\neyr:2028 hcl:#c0946f hgt:73in byr:1926 ..
```

Different fields of a single entry are separated by either spaces, or newlines, so we'll have to first group the entries appropriately. The good thing is that k has some string splitting primitives built in!

```k
 :3# i:({(`$x)!y}/ + ":"\' ,/ " "\' "\n"\)' "\n\n"\ 1:"./aoc/2020/day04.txt"
(`hgt`iyr`hcl`ecl`byr`eyr`cid`pid!("176cm";"2013";"#fffffd";"amb";"2000";"2034";"89";"934693255")
 `hcl`ecl`hgt`pid`iyr`byr`eyr!("#b5c3db";"grn";"155cm";"#baec97";"2017";"1939";"2020")
 `pid`eyr`hgt`ecl`byr`hcl`iyr!("526669252";"1972";"152cm";"dne";"1960";,"z";"2023"))
```

Split by group (`\n\n`), and then for each group split by newline and space, combine the result into a single list (via the *raze* idiom `,/`), and, split on a semicolon. Also, make that whole thing a dictionary, because k natively supports that!

TODO: more

```k
 :fst:*i
!/+((`"hgt";"176cm")
    (`"iyr";"2013")
    (`"hcl";"#fffffd")
    (`"ecl";"amb")
    (`"byr";"2000")
    (`"eyr";"2034")
    (`"cid";"89")
    (`"pid";"934693255"))
 fst@`hgt / can query by key
"176cm"
 !fst     / get all keys
(`"hgt"
 `"iyr"
 `"hcl"
 `"ecl"
 `"byr"
 `"eyr"
 `"cid"
 `"pid")
```

The main part is finding out whether all of the required keywords are there, and to ignore `cid`. This just means using *without* (`^`) and seeing if the resulting list is of length 0.

```k
 +/ vs:(0= #`byr`iyr`eyr`hgt`hcl`ecl`pid^ !:)'i / valid?]
230
```

---

Security is onto us! Some passports are getting through that obviously should not, so it might be a good idea to implement some kind of validation after all. Here are the rules:

> -   `byr` (Birth Year) - four digits; at least 1920 and at most 2002.
> -   `iyr` (Issue Year) - four digits; at least 2010 and at most 2020.
> -   `eyr` (Expiration Year) - four digits; at least 2020 and at most 2030.
> -   `hgt` (Height) - a number followed by either cm or in:
>     -   If cm, the number must be at least 150 and at most 193.
>     -   If in, the number must be at least 59 and at most 76.
> -   `hcl` (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
> -   `ecl` (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
> -   `pid` (Passport ID) - a nine-digit number, including leading zeroes.
> -   `cid` (Country ID) - ignored, missing or not.

```k
In:{((x=*|y)|x<*|y)&((x=*y)|x>*y)} / x ∈ [y₁, y₂]? k has no ≤ or ≥ by default 🙃 XXX
Ap:{(z x y)&(~&/^x y)}; Aps:{&/{Ap[x;*y;*|y]}[x]'y}
Test:{Aps[x;((`byr; {In[.x; 1920 2002]})
             (`iyr; {In[.x; 2010 2020]})
             (`eyr; {In[.x; 2020 2030]})
             (`hgt; {:["cm"~-2#x;In[.-2_x;150 193]; "in"~-2#x;In[.-2_x;59 76]; 0]})
             (`hcl; {(7=#x) & (0=#(1_x)^("a"+!6),("0"+!10)) & ("#"=*x)})
             (`ecl; (~^("amb";"blu";"brn";"gry";"grn";"hzl";"oth")?))
             (`pid; {(9=#x) & (0=#x^("0"+!10))}))]}
```

```k
 +/Test'i@&vs
156
```

---

Full solution:


## [Day 5](https://github.com/slotThe/advent/blob/master/aoc2020/k-solutions/day05.k)

Today we lost our boarding pass, and have to find our seat by process of elimination. The input gives a binary space partitioning of the plane for each passenger:

```k
 5# 0:"./aoc/2020/day05.txt"
("BFFFBBFRLR"
 "FBFFFBBLRL"
 "BFFFFFBRRR"
 "FBBBFFFLRR"
 "FFBFBFBRLL")
```

Our first task is to find the highest seat ID. This is described in a wonderfully obfuscated way of translating the first seven digits into a row `r`, the last three digits into a column `c`, and then doing `c + 8*r`. Of course, what this is actually saying is that we have to calculate the binary value of the string, with `B` and `R` being treated as 1.

Since K does not seem to have an "element in", this is actually not as straightforward as I would like it to be. For example, in BQN we could just use `∊⟜"BR"` and be fine:

```bqn
   ∊⟜"BR"¨ 5↑ •FLines "./aoc/2020/day05.txt"
⟨ ⟨ 1 0 0 0 1 1 0 1 0 1 ⟩ ⟨ 0 1 0 0 0 1 1 0 1 0 ⟩ ⟨ 1 0 0 0 0 0 1 1 1 1 ⟩ ⟨ 0 1 1 1 0 0 0 0 1 1 ⟩ ⟨ 0 0 1 0 1 0 1 1 0 0 ⟩ ⟩
```

The easiest way I've found to achieve this in the general case is to just write an `elem` function:

```k
elem:{~^(y?)'x} / x elem y
```

```k
 elem[2+!10       ; 5 7 0]
0 0 0 1 0 1 0 0 0 0
 elem[5 7 0       ; 2+!10]
1 1 0
 elem["BR"        ; "BFFFBBFRLR"]
1 1
 elem["BFFFBBFRLR"; "BR"]
1 0 0 0 1 1 0 1 0 1
```

The fact that functions are nouns in K and thus have to be applied via M-Expressions is, ergonomically, not super great for functions that are meant to be read infix. As I said before, there is the option to define the actual Unicode character `∈` for this, which can be used as a verb, but I will also not make use of this feature here.

A different solution would also be to use *each left* for checking equality with either of the two letters, and then or the whole thing together:

```k
 |/ "BR"=\: 10# 0:"./aoc/2020/day05.txt"
(1 0 0 0 1 1 0 1 0 1
 0 1 0 0 0 1 1 0 1 0
 1 0 0 0 0 0 1 1 1 1
 0 1 1 1 0 0 0 0 1 1
 0 0 1 0 1 0 1 1 0 0
 0 1 1 0 0 1 1 0 1 1
 0 1 1 0 0 0 0 1 0 0
 1 0 0 0 1 1 0 1 0 0
 1 1 0 1 0 1 0 1 0 0
 1 0 0 0 0 0 1 1 0 0)
```

This at least only uses builtin verbs, but to be honest I find this pretty difficult to read. `elem` it is.

```k
 :i: 2/' elem[0:"./aoc/2020/day05.txt"; "BR"]
565 282 527 451 172 411 388 564 852 524 143 805 816 336 769 228 151 562 755 656 154 165 185 105 114 368 393 572 222 313 358 480 161 529 467 263 182 569 404 540 454 833 437 79 730 459 819 216 638 428 518 848 289 341 701 817 599 720 365 426 291 519 532 321 530 772 323 60..
```

After parsing, we can use yet another overload of the forward slash—an (array of) integer(s) as its left argument turns it into *decode*.

```k
 |/ i    / part one
864
```

---

For part two, we notice that the flight is completely full, with a slight caveat. There are some additional seats in the very front and back, which are not available in the airplane. We have to somehow find the only seat "in the middle" of a row that's not taken yet.

```k
 :ms:(!1024)^i            / missing seats
0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 739 865 866 867 868 869 870 871 872 873 874 875 876 87..
 ms[* 1_ &0= {y=x-1}':ms] / value of first middle seat
739
```

---

Full solution:

```k
elem:{~^(y?)'x} / x ∈ y
i: 2/' elem[0:"./aoc/2020/day05.txt"; "BR"]
864= |/i
739= ms@*1_&0={y=x-1}':ms:(!1024)^i
```


# Days 6–10


## Day 6

Today, we're helping the other passengers on the plane navigate ridiculous customs forms. All passengers are paired into groups, and given a list of 26 questions each. Part one asks us to identify the questions for which anyone in the group answered "yes". The input is grouped by group, as well as individual inside of the group (only the "yes" answers are recorded).

```k
 1:"./aoc/2020/day06.txt"
"gsvdkufnoawjmhp\nwvhusojpnikgfadb\nvshnpfedgwajkou\naujodhskfvnpgw\nokpdnwhsfvjguqa\n\njuedvq\nvqeduj\nveqdju\neqduvj\n\ncdajbuernxm\nmnucjearxbd\naxrmdejuncb\njrebucmdxna\n\nkutplibdoqzfvhw\nqcewmrkdvhl\nqobdwvlkgxhpasyjn\n\npsbjhnatxe\npnaxesjhbt\ntpxhbnseaj\n\nyskv..
 ,"\n\n"\ 1:"./aoc/2020/day06.txt"              / By groups
,("gsvdkufnoawjmhp\nwvhusojpnikgfadb\nvshnpfedgwajkou\naujodhskfvnpgw\nokpdnwhsfvjguqa";"juedvq\nvqeduj\nveqdju\neqduvj";"cdajbuernxm\nmnucjearxbd\naxrmdejuncb\njrebucmdxna";"kutplibdoqzfvhw\nqcewmrkdvhl\nqobdwvlkgxhpasyjn";"psbjhnatxe\npnaxesjhbt\ntpxhbnseaj";"yskvjqa..
 5# i: "\n"\' "\n\n"\ 1:"./aoc/2020/day06.txt" / Individual groups by line
(("gsvdkufnoawjmhp";"wvhusojpnikgfadb";"vshnpfedgwajkou";"aujodhskfvnpgw";"okpdnwhsfvjguqa")
 ("juedvq";"vqeduj";"veqdju";"eqduvj")
 ("cdajbuernxm";"mnucjearxbd";"axrmdejuncb";"jrebucmdxna")
 ("kutplibdoqzfvhw";"qcewmrkdvhl";"qobdwvlkgxhpasyjn")
 ("psbjhnatxe";"pnaxesjhbt";"tpxhbnseaj"))
```

Now we can just concatenate each group, and use *distinct*, `?X`, to get the unique characters of the string.

```k
 ,/*i
"gsvdkufnoawjmhpwvhusojpnikgfadbvshnpfedgwajkouaujodhskfvnpgwokpdnwhsfvjguqa"
 ?,/*i
"gsvdkufnoawjmhpibeq"
 #?,/*i
19
 +/(#?,/)'i / Part one
6335
```

---

Of course, we've actually misread the instructions! Instead of identifying the questions to which anyone answered "yes", we need to identify those to which *everyone* answered "yes". This essentially involves intersecting all of the answers for a given group, and just counting what pops out. K does not have list intersection as a primitive, but it does have `\setminus`, in the form of `X^y`.

```k
 {x^x^y}/*i      / x^x^y is x∩y
"gsvdkufnoawjhp"
 +/(#{x^x^y}/)'i / Part two
3392
```

---

Full solution:

```k
i: "\n"\' "\n\n"\ 1:"./aoc/2020/day06.txt"
6335= +/(#?,/)'i
3392= +/(#{x^x^y}/)'i
```


## Day 7

We're trying to catch our connection at a regional airport, but the flights are delayed due to some luggage processing issues! Let's see if we can't fix this.

```k
 5# i: " "\' 0:"./aoc/2020/day07.txt"
(("plaid";"fuchsia";"bags";"contain";,"5";"light";"violet";"bags,";,"1";"light";"yellow";"bag.")
 ("striped";"aqua";"bags";"contain";,"2";"striped";"teal";"bags.")
 ("clear";"coral";"bags";"contain";,"2";"plaid";"green";"bags,";,"5";"mirrored";"gold";"bags.")
 ("dull";"tan";"bags";"contain";,"4";"faded";"blue";"bags,";,"3";"faded";"olive";"bags,";,"5";"dull";"salmon";"bags.")
 ("plaid";"green";"bags";"contain";,"3";"faded";"green";"bags."))
```

I will not write a general parser, but use lots of properties of the input, like that bag names are always two words.

TODO: a thing about typed nulls here

```k
 ,:f:*i                                   / first line, for testing
,("plaid";"fuchsia";"bags";"contain";,"5";"light";"violet";"bags,";,"1";"light";"yellow";"bag.")
 0N 4#f                                   / Partition
(("plaid";"fuchsia";"bags";"contain")
 (,"5";"light";"violet";"bags,")
 (,"1";"light";"yellow";"bag."))
 {`$,/2#*x} 0N 4#f                        / Symbols!
`plaidfuchsia
 {(`$,/2#*x; {(.*x;`$,/2#1_x)}'1_x)} 0N 4#f / Draw the rest of the owl
(`"plaidfuchsia"
 ((5;`lightviolet);(1;`lightyellow)))
```

This is *almost* correct: the only thing we still need to add are bags that have a description like

> pale brown bags contain no other bags.

Again, having rather strong assumptions about the shape of the input helps.

```k
 {"no"~x 4}@ " "\"pale brown bags contain no other bags."
1
 {"no"~x 4}@ f
0
```

With that, we can write our parser:

```k
P:{p:0N 4#x; n:`$,/2#*p
   $["no"~x 4; (n;()); {(x; {(.*x;`$,/2#1_x)}'1_y)}[n]p ]}
```

```k
 P@f
(`"plaidfuchsia"
 ((5;`lightviolet);(1;`lightyellow)))
 P@ " "\"pale brown bags contain no other bags."
(`"palebrown"
 ())
 5# i:!/ + P' " "\' 0:"./aoc/2020/day07.txt"
!/+((`"plaidfuchsia";((5;`lightviolet);(1;`lightyellow)))
    (`"stripedaqua" ;,(2;`stripedteal))
    (`"clearcoral"  ;((2;`plaidgreen);(5;`mirroredgold)))
    (`"dulltan"     ;((4;`fadedblue);(3;`fadedolive);(5;`dullsalmon)))
    (`"plaidgreen"  ;,(3;`fadedgreen)))
```

To find all bags that could contain `` `shinygold ``, we proceed recursively.

```k
/ Can bag y be contained in bag x, given the bags i?
CanCon:{bs:(i y)@'1; $[0=#bs;0; ~^bs?x;1; |/o[x;]'bs]}
```

```k
 CanCon[`dullwhite;`plaidfuchsia]
1
 +/CanCon[`shinygold;]'!i             / Part one
205
```

---

Part two asks us how many bags are actually contained in our shiny gold bag—the answer to which is rightfully called "topologically impractical" :)

Implementation-wise, we again do nothing but write a simple recursive function.

```k
 Cnt:{bs:i x; +/$[0=#bs; 0; {x+x*Cnt[y]}/'bs]}
 Cnt[`shinygold]                       / Part two
80902
```

I don't like that both days have this ugly base case of `0=#bs`, but I also don't really know how to get rid of it… Suggestions welcome!

---

Full solution:

```k
 P:{p:0N 4#x; n:`$,/2#*p; $["no"~x 4; (n;()); {(x; {(.*x;`$,/2#1_x)}'1_y)}[n]p ]}
 i:!/+P'" "\'0:"./aoc/2020/day07.txt"
 205  = +/{bs:(i y)@'1; $[0=#bs;0; ~^bs?x;1; |/o[x;]'bs]}[`shinygold;]'!i
1
 Cnt:{bs:i x; +/$[0=#bs; 0; {x+x*Cnt[y]}/'bs]}
 80902= Cnt[`shinygold]
1
```


## Day 8

Today we are just sitting on our flight, minding our own business, when suddenly… the game boy of the kid next to us stops working. Obviously, the problem is a boot loop, so we'll have to investigate further.

Our input is the boot code of the game boy, which in itself consists of three operations, each with a mandatory argument: `acc`, `jmp`, and `nop`. Before proceeding, let's quickly parse this.

```k
 :i: {(`$x 0;.(x 1)^"+")}' " "\' 0:"./aoc/2020/day08.txt"
((`acc;-7)
 (`acc;2)
 (`acc;20)
 (`acc;14)
 (`jmp;191)
 (`acc;47)
 (`nop;339)
 (`acc;49)
 (`jmp;104)
 (`jmp;629)
 (`jmp;374)
 (`acc;24)
 (`jmp;220)
 (`nop;474)
 (`acc;25)
 (`jmp;340)
 (`acc;16)
 (`acc;3)
 (`acc;41)
 (`jmp;566)
 (`jmp;296)
 ..)
```

To execute this, we just need to get ourselves a single global value for the `add` instruction, as well as a program counter for the `jmp`. The `nop` instruction does nothing. Part one asks us to execute this instruction set, and find the value of the global value before that instruction executes.

```k
 / value, counter, seen, using "while" f f/
 *{(v;c;s):x;^s?c}{(v;c;s):x; (a;b):i c; (v+b*`acc=a; c+(1;b)@`jmp=a; s,c)}/(0;0;,0N)
1594
```

Notation is a bit awkward, since K (well, at least `ngn/k`) does not support pattern matching in function headers, and I haven't found a good way around this yet. Oh well.

---

Against all odds, the noop is actually a noop! However, we get the prescient feeling that exactly one `jmp` instruction should be a `nop`, or the other way around, so that the program can exit cleanly. We just have to find out which instruction to flip.

Let's see how many different paths we'd have to check.

```k
 #&|/`jmp`nop=\:*'i / How many jumps or noops?
291
```

This code uses the `|/X=\:` idiom which I skipped for readability reasons on day 5—funny how these things work. Anyways, with just ~300 simulations, this part seems destined to be brute-forced.

```k
 is:@[i;;{(*`jmp`nop^x 0;x 1)}]'&|/`jmp`nop=\:*'i; / instructions
 mx:-1+#i                                          / max instruction count
 While:{(v;c;s;j):x;(^s?c)&(c<mx)}                 / loop while this is true
 Do:{(v;c;s;j):x; (a;b):j c; (v+b*`acc=a; c+(1;b)@`jmp=a; s,c; j)}
 **{mx=x@'1}# {While Do/(0;0;,0N;x)}'is            / filter for successful exit
758
```

---

Full solution:

```k
i:{(`$x 0;.(x 1)^"+")}' " "\' 0:"./aoc/2020/day08.txt";  mx:-1+#i
While:{(v;c;s;j):x; (^s?c)&(c<mx)}
Do:{(v;c;s;j):x; (a;b):j c; (v+b*`acc=a; c+(1;b)@`jmp=a; s,c; j)}
Sol:{While Do/(0;0;,0N;x)}
1594= *           Sol i
758 = **{mx=x@'1}#Sol'@[i;;{(*`jmp`nop^x 0;x 1)}]'&|/`jmp`nop=\:*'i
```


## Day 9

After using several paperclips to connect our computer to our in-flight screen, we're trying to break the encryption of its open data port.

```k
 :i:.'0:"./aoc/2020/day09.txt" / .C is eval
11 6 42 19 23 20 26 4 2 36 35 41 18 38 28 1 48 5 17 10 32 15 46 50 24 3 7 6 8 68 83 23 11 16 4 36 9 78 12 29 13 25 42 14 84 10 17 15 18 28 19 21 20 22 35 24 45 30 67 50 23 40 27 26 31 38 33 25 42 29 32 34 57 41 70 43 44 46 65 75 47 48 89 49 51 61 60 52 54 56 99 86 90 7..
```

The data is encrypted with the whimsically named "eXchange-Masking Addition System"; it starts off by transmitting 25 numbers, and after that every number that follows is a sum of two of the previous 25 numbers.

```k
 #i
1000
```

Only 1000 numbers means we're probably able to just brute-force this, in a similar way to what we did for day 1.

```k
 :p1: i@ 25+ *& {~|//(*|x)=as+/:as:-1_x}' 26':i / Part one
1930745883
```

Here, I used *windows* (`i':`) and *converge* (`f/`), which I think are pretty self-explanatory. Indeed, using converge with a fold as a function is relatively common for doing things like flattening arrays:

```k
 5 5#!25    / Array
(0 1 2 3 4
 5 6 7 8 9
 10 11 12 13 14
 15 16 17 18 19
 20 21 22 23 24)
 +/5 5#!25  / Column rums
50 55 60 65 70
 +//5 5#!25 / Array sum
300
```

---

Having found `p1`, we are now tasked to find a *contiguous* set of two numbers that sum up to it. Problems like this can usually quite neatly be solved using a simple sliding window approach: start with a window size at the start of the array, grow the window as long as the sum is smaller or equal to the target, and shrink the window (from the back) when it's bigger.

This additional complexity is not actually needed here, and I'm just going to brute-force this: calculate the full sum scan of `i`, `1_i`, and so on and find the correct offset based on that. That's only, like, 10000 numbers to check, which sounds fast enough to do that instead.

```k
 {+\x_t}'!#t:35 20 15 25 47 40 62 / Test input
(35 55 70 95 142 182 244
 20 35 60 107 147 209
 15 40 87 127 189
 25 72 112 174
 47 87 149
 40 102
 ,62)
 +&{127=+\x_t}'!#t                / Gets the row and offset
,2 3
```

```k
 :(r;of): *+&{p1=+\x_i}'!#i / Now for the actual input
553 16
 {(&/x)+|/x}(of+1)#r_i      / Part two
268878261
```

---

Full solution:

```k
i:.'0:"./aoc/2020/day09.txt"
1930745883= p1: i@ 25+ *&{~|//(*|x)=as+/:as:-1_x}' 26':i
(r;of): *+&{p1=+\x_i}'!#i
268878261 = {(&/x)+|/x}(of+1)#r_i
```


## Day 10

Just as we discover the weather forecasts of a massive tropical storm, our battery dies! We'll have to charge it using the weird charging outlets on the plane. We'll use a number of adapters that we have with us to transform the joltage levels to something our computer can work with. Our puzzle input consists of the output joltage each adapter can handle.

```k
 :i:i@<i:.'0:"./aoc/2020/day10.txt"
1 2 3 4 7 10 11 12 13 14 17 18 19 20 21 24 25 26 27 28 31 32 33 34 35 38 39 40 43 46 47 48 49 52 53 54 55 56 59 62 63 64 65 66 69 72 73 76 77 78 81 82 83 84 87 88 89 90 93 94 95 96 99 102 103 104 105 108 109 110 111 114 117 120 121 122 123 126 127 128 129 132 135 138 1..
 #i
101
```

Additionally, our device has a built-in adapter three jolts higher than the highest one in the input. The outlet has joltage level 0. Every adapter can only be connected to a source 1-3 jolts lower than its rating. Part one asks us to use every adapter in the bag (plus the one we have), count the number of 1-jolt and 3-jolt differences, and add them up.

Since we have to use every adapter once, there's not actually much to do here. We just have to compute how far each element is away from its predecessor, which is conveniently done with *eachprior* `F':`:

```k
 :d:3+|/i
164
 -':i,d         / Differences, initial 0 is implicit
1 1 1 1 3 3 1 1 1 1 3 1 1 1 1 3 1 1 1 1 3 1 1 1 1 3 1 1 3 3 1 1 1 3 1 1 1 1 3 3 1 1 1 1 3 3 1 3 1 1 3 1 1 1 3 1 1 1 3 1 1 1 3 3 1 1 1 3 1 1 1 3 3 3 1 1 1 3 1 1 1 3 3 3 1 1 1 1 3 1 1 1 3 1 1 1 1 3 1 1 1 3
 =-':i,d        / Group =X
!/+((1;0 1 2 3 6 7 8 9 11 12 13 14 16 17 18 19 21 22 23 24 26 27 30 31 32 34 35 36 37 40 41 42 43 46 48 49 51 52 53 55 56 57 59 60 61 64 65 66 68 69 70 74 75 76 78 79 80 84 85 86 87 89 90 91 93 94 95 96 98 99 100)
    (3;4 5 10 15 20 25 28 29 33 38 39 44 45 47 50 54 58 62 63 67 71 72 73 77 81 82 83 88 92 97 101))
 */ #' =-':i,d / Part one
2201
```

---

Instead of some terrible optimisation problem, part two involves counting the number of ways that we can connect the outlet `0` to our device `d`, now not necessarily using every adapter. Essentially, every time there is a `1` instead of a `3` in the differences above, we have the opportunity to leave out that specific adapter. Obviously—and in fact, we're even told that our solution will be more than a trillion—this results in a bit of a combinatorial explosion; we'll have to come up with a smart plan instead.

Looking at the differences again, we see a curious pattern:

```k
 -':i,d
1 1 1 1 3 3 1 1 1 1 3 1 1 1 1 3 1 1 1 1 3 1 1 1 1 3 1 1 3 3 1 1 1 3 1 1 1 1 3 3 1 1 1 1 3 3 1 3 1 1 3 1 1 1 3 1 1 1 3 1 1 1 3 3 1 1 1 3 1 1 1 3 3 3 1 1 1 3 1 1 1 3 3 3 1 1 1 1 3 1 1 1 3 1 1 1 1 3 1 1 1 3
```

All adapters have distance one or three, no twos in sight. The threes thus function as a sort of "reset point": they can't themselves be touched by the surrounding adapters, so we can treat each group of ones as its own little island. The rules for each island are simple: we can't remove anything from the front or back, and in the middle we can remove at most two numbers. For example:

```
  3 1 1 1 3     # This is a possible "island" of offsets
-3 0 1 2 3 6    # All of these would be valid
-3 0 1   3 6
-3 0   2 3 6
-3 0     3 6
```

This is the number of binary sequences with at most two ones, which can for example be calculated using $\sum_{i=0}^2 {n \choose i}$, where $n$ is the length of the sequence. Since our sequences are only of length one, two, three, or four, I'll just hard-code the only two numbers we need to care about here: `7=1+3+3` for `3 1 1 1 1 3` and `4=1+2+1` for `3 1 1 1 3`.

```k
 ,"3"\,/$-':i,d                             / Better solutions welcome!
,("1111";"";"1111";"1111";"1111";"1111";"11";"";"111";"1111";"";"1111";"";,"1";"11";"111";"111";"111";"";"111";"111";"";"";"111";"111";"";"";"1111";"111";"1111";"111";"")
 */ {$[x=4;7;x=3;4;1|x]}' #' "3"\,/$-':i,d / Part two
169255295254528
```

---

Full solution:

```k
i:i@<i:.'0:"./aoc/2020/day10.txt"
2201= */#'=d:-':i,3+|/i
169255295254528= */{$[x=4;7;x=3;4;1|x]}'#'"3"\,/$d
```


# Days 11–15


## Day 11


## Day 12

```k
 :i:{(`$*x;.1_x)}'0:"./aoc/2020/day12.txt"
((`W;1)
 (`F;91)
 (`W;3)
 (`F;82)
 (`N;1)
 (`E;2)
 (`N;4)
 (`R;90)
 (`F;25)
 (`N;2)
 (`F;75)
 (`E;4)
 (`R;90)
 (`F;91)
 (`R;90)
 (`F;64)
 (`L;90)
 (`E;1)
 (`L;90)
 (`S;2)
 (`L;180)
 ..)
```

```k
 p::0 0; d::0 1; rot:{((-y;x);(-x;-y);(y;-x))}; T:{(rot/d)@90 180 270?x};
 {$[`N=x;p+:y*-1 0; `S=x;p+:y*1 0; `W=x;p+:y*0 -1; `E=x;p+:y*0 1; `F=x;p+:y*d; `L=x;d::T[y]; `R=x;d::T[360-y]]}/'i;
 +/p|-p
1294
```

```k
 wp::-1 10; p::0 0; d::0 1; rot:{((-y;x);(-x;-y);(y;-x))}; T:{(rot/wp)@90 180 270?x};
 {$[`N=x;wp+:y*-1 0; `S=x;wp+:y*1 0; `W=x;wp+:y*0 -1; `E=x;wp+:y*0 1; `F=x;p+:y*wp; `L=x;wp::T[y]; `R=x;wp::T[360-y]]}/'i;
 (wp;p;d)
(10 -6
 2726 -17866
 0 1)
 +/p|-p
20592
```


## Day 13

We are about to reach a nearby port, only to discover that no ships actually go to our vacation island. Time for another plane.[^11] Thankfully, there is a whole array of buses that go to and from the airport—each bus ID indicates the intervals at which it leaves, starting from a fixed timestamp `0` at which every bus leaves (must've been quite the spectacle!).

```k
 :(t;bf):{`I$'(x 0;","\x 1)}@0:"./aoc/2020/day13.txt" /timestamp and busses filled
(1008832
 23 0N 0N 0N 0N 0N 0N 0N 0N 0N 0N 0N 0N 41 0N 0N 0N 0N 0N 0N 0N 0N 0N 449 0N 0N 0N 0N 0N 0N 0N 0N 0N 0N 0N 0N 0N 0N 0N 0N 0N 13 19 0N 0N 0N 0N 0N 0N 0N 0N 0N 29 0N 991 0N 0N 0N 0N 0N 37 0N 0N 0N 0N 0N 0N 0N 0N 0N 0N 17)
 :ib:&~^bf                                            /indices of busses
0 13 23 41 42 52 54 60 71
 :bs:bf^0N                                            /busses
23 41 449 13 19 29 991 37 17
```

The first number is the earliest timestamp at which we would be able to leave. The first part of the question asks us to find the unique earliest bus that would take us to the airport, and multiply that by how many minutes after the given timestamp that bus leaves. This isn't so bad: starting from timestamp $t$, we're looking for that bus $b$ for which $t-b \mod b$ is minimal.

```k
 {x!x-t}'bs                 /all offsets
17 14 71 7 11 20 6 10 16
 &/{x!x-t}'bs               /minimum
6
 is?&/is:{x!x-t}'bs         /index of minimum
6
 {x*bs@x}is?&/is:{x!x-t}'bs /part one
5946
```

I believe I've never used *find*, `X?y`, before this point, but it very much does what it says on the tin: it looks through the array to find the first occurrence of the given element, and either returns the index, or NULL if the element is not in the array.

```k
 1 2 3 4 ? 4 1 0 6 7
3 0 0N 0N 0N
 "abc"?"ad"
0 0N
```

---

It's a maths day—rejoice!

Having found our bus, we're taking part in the shuttle companies' contest:

> one gold coin for anyone that can find the earliest timestamp such that the first bus ID departs at that time and each subsequent listed bus ID departs at that subsequent minute. […] An `x` in the schedule means there are no constraints on what bus IDs must depart at that time.

So given something like `7,13,x,x,59,x,31,19` we must have that ID `7` departs at timestamp `t`, `13` departs at `t+1`, `59` departs at `t+4`, and so on.

If you stare at this enough, you conveniently realise that the numbers above are all prime. In particular, we are looking for a number $t$, such that

$$\begin{aligned} t &\equiv 0 \mod 7 \\ t &\equiv 1 \mod 13\\ t &\equiv 4 \mod 59\\ t &\equiv 6 \mod 31\\ t &\equiv 7 \mod 19 \end{aligned} $$

If you're thinking of the [Chinese remainder theorem](https://en.wikipedia.org/wiki/Chinese_remainder_theorem) at this point, you'd be exactly right! If a single line is $t \equiv a_i \mod n_i$, then the usual algorithm to find a solution goes like this:

1.  Define $N = \mathrm{lcm}\,{\{n_i\}}_i$; if all $n_i$ are prime (which is the case here), this is just their product.
2.  Compute $m_i = a_i / N$ for all $i$.
3.  Compute the inverse $x_i$ of $m_i$ mod $n_i$ for all $i$.
4.  The solution is any $t \equiv \sum_i a_i \cdot m_i \cdot x_i \mod N$.

Our K solution will follow this algorithm; the $a_i$ are just the bus IDs minus their indexes, and the rest translates essentially verbatim.

```k
 ai:bs-ib
 mi:(-bs)!'N:*/bs            /Defining N here
 xi:*'&'1=bs!'(!'bs)*bs!'mi
 N!+/ai*mi*xi /part two
645338524823718
```

---

Full solution:

```k
(t;bf):{`I$'(x 0;","\x 1)}@0:"./aoc/2020/day13.txt"; ib:&~^bf; bs:bf^0N
5946= {x*bs@x}is?&/is:{x-x!t}'bs
ai:bs-ib; mi:(-bs)!'N:*/bs; xi:*'&'1=bs!'(!'bs)*bs!'mi
645338524823718= N!+/ai*mi*xi
```


## Day 14

We're now really, really, about to reach the sea port. However, the captain needs our help to fix the docking program on the ferry, so that it works with the port's computer system. The system initialises some memory using a custom bitmask system, which we'll need to emulate in software.

The input looks like this:

```
mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1
```

A line defining the current mask, followed by one or more lines of memory assignments. The mask is given in big endian order, and acts upon the values of the memsets, which we'll have to transform to binary before modifying them. A `0` or a `1` in the mask means to override the bit of the value in the respective position, while an `X` indicates no change.

I think this is most elegantly done with two masks: one for all of the zeros, and one for all of the ones. Let's write a parsing function that sets two global variables, `gz` and `go`:

```k
 "01"=\:"XXX000111X"
(0 0 0 1 1 1 0 0 0 0
 0 0 0 0 0 0 1 1 1 0)
 Msk:{(gz;go)::"01"=\:7_x} /drop "mask = " prefix
 Msk "mask = 000000000000000000000000000000X1001X"; (gz;go)
(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0)
```

Hackily parsing a memset line is relatively straightforward:

```k
 en:`I$ 4 3_' "]"\ "mem[42] = 100" /extract numbers
 {((36-#a)#0),a:2\y}/en           /pad 2nd number to 36 bits
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1 0 0
```

Setting the memory works with another global variable; a dict that holds associations of a memory address with its value.

```k
 mem::(!0)!!0
 Mem:{mem[x]:2/(~gz)&go|((36-#a)#0),a:2\y}/`I$4 3_'"]"\
 Msk "mask = 000000000000000000000000000000X1001X";
 Mem "mem[42] = 100"
 mem
(,42)!,50
```

The maths seems to check out, at least:

```
value:  000000000000000000000000000001100100  (dec 100)
mask:   000000000000000000000000000000X1001X
result: 000000000000000000000000000000110010  (dec 50)
```

Now that we have the two functions, we just need to evaluate the input line by line, and sum up all memory values at the end:

```k
 mem::(!0)!!0
 {$["mask"~4#x;Msk x;Mem x]}'0:"./aoc/2020/day14.txt";
 +/mem
10452688630537
```

---

We simulated the wrong decoder chip, of course, and instead need to program version two! Instead of the memory address, the mask now works on the value of the memset. Also, the rules are slightly different: now, we have to change the memory address, and `0` leaves the bit unchanged, `1` overwrites it with `1`, and an `X` creates two separate memory writes: one for the string with `X` replaced by `0`, and one for `1`.[^12] In particular, the mask is first applied to the value before we can begin "processing" all of the alternatives.

When parsing this, we can make use of the fact that, as opposed to something like BQN's `•BQN`, K's evaluation via `.C` works inside the current scope. In particular, we can try to define `X` to be any value we want, and evaluate the string character-by-character to immediately turn it into an integer array! Hacky, certainly, but the fun kind of hacky!

```k
 X:2
 .'"XXX000111X" /eval each
2 2 2 0 0 0 1 1 1 2
```

Assuming we have a string like `2220001112`, the strategy that I went with is to just split at every `2` and create two new strings, one for each possible outcome.

```k
 s:1 0 2 2 0 0 0 1 1 1 2
 &X=s                               /indices of 2's
2 3 10
 1_'(b:&X=s)_s                      /cut at indices and drop 2's
(!0
 0 0 0 1 1 1
 !0)
 (0 1,\:1_)'(b:&X=s)_s              /two possible worlds
((,0;,1)
 (0 0 0 0 1 1 1;1 0 0 0 1 1 1)
 (,0;,1))
 {,/(x,\:)'y}/(0 1,\:1_)'(b:&X=s)_s /combine to all possible combs
(0 0 0 0 0 1 1 1 0
 1 0 0 0 0 1 1 1 0
 0 1 0 0 0 1 1 1 0
 1 1 0 0 0 1 1 1 0
 0 0 0 0 0 1 1 1 1
 1 0 0 0 0 1 1 1 1
 0 1 0 0 0 1 1 1 1
 1 1 0 0 0 1 1 1 1)
 (s[!*b],)'{,/(x,\:)'y}/(0 1,\:1_)'(b:&X=s)_s /add back first part
(1 0 0 0 0 0 0 1 1 1 0
 1 0 1 0 0 0 0 1 1 1 0
 1 0 0 1 0 0 0 1 1 1 0
 1 0 1 1 0 0 0 1 1 1 0
 1 0 0 0 0 0 0 1 1 1 1
 1 0 1 0 0 0 0 1 1 1 1
 1 0 0 1 0 0 0 1 1 1 1
 1 0 1 1 0 0 0 1 1 1 1)
```

So, all in all we have:

```k
Cs:{(2/x[!*a],)'{,/(x,\:)'y}/(0 1,\:1_)'(a:&X=x)_x} /combinations
```

I also added in a `2/` at the very end, so that we get decimal numbers out of `Cs`, instead of binary strings.

The `Mem` function for part two looks essentially like the first one, only we do the bitstring processing on the address part, instead of the value one.

```k
 X:2; Msk:{msk::.'7_x}
 mem::(!0)!!0
 Mem:{mem,: ![;y] Cs msk|((36-#a)#0),a:2\x}/`I$4 3_'"]"\
 Msk "mask = 000000000000000000000000000000X1001X"
 Mem "mem[10] = 94"
 mem
!/+((26;94)
    (58;94)
    (27;94)
    (59;94))
```

Execution works exactly as before.

```k
 mem::(!0)!!0
 {$["mask"~4#x;Msk x;Mem x]}'0:"./aoc/2020/day14.txt";
 +/mem /part two
2881082759597
```

---

Full solution:

```k
 mem:(!0)!!0; X:2; mem2:(!0)!!0
 Cs:{(2/x[!*a],)'{,/(x,\:)'y}/(0 1,\:1_)'(a:&X=x)_x} /combinations
 Msk:{(gz;go)::"01"=\:7_x}; Msk2:{msk::.'7_x}
 Mem:{mem[x]: 2/(~gz)&go|((36-#a)#0),a:2\y}/`I$4 3_'"]"\
 Mem2:{mem2,:![;y]Cs msk|((36-#a)#0),a:2\x}/`I$4 3_'"]"\
 {$["mask"~4#x;[Msk x;Msk2 x];[Mem x;Mem2 x]]}'0:"./aoc/2020/day14.txt";
 10452688630537 2881082759597~ +/'(mem;mem2)
1
```


## Day 15

We caught the airport shuttle, only to find out that all direct flights have been cancelled. But no worries, we'll just take a different route—can't be that bad, right?

While waiting for that alternative flight, we're playing a memory game with the elves at the North Pole. Our puzzle input, and every player in this game says a number, according to the following rules:

> -   If that was the first time the number has been spoken, the current player says 0.
> -   Otherwise, the number had been spoken before; the current player announces how many turns apart the number is from when it was previously spoken.

Parsing is straightforward:

```k
 :i:|.'","\1:"./aoc/2020/day15.txt"
2 1 19 0 11 8
```

The actual algorithm for this problem isn't super complicated: For the first number of the input, check if it's already somewhere in the array. If not, add 0 to the front, otherwise use the index of the last occurrence incremented by one.

```k
 447= *(2020-#i){(1+-1^(1_x)?*x),x}/i / part one
1
```

---

For part two, the elves want to know how well we can optimise this simple loop! They task us with finding the 30'000'000th number said aloud. To solve this efficiently, we can replace the array above by just a dict—store the index of the last occurrence of each number, since we don't need more when calculating the next value. The fact that one starts with a number that's already in the dict can throw one into a bit of a loop, but there's not much special casing necessary, actually: if a number is found in step `n`, then the new number to be inserted is `n-i`, where `i` is the index of the number. While the very first number will already be found in the dict, its index `i` is exactly `n`, so we can just treat it as if it wasn't in the dict at all.

```k
 i:.'","\1:"./aoc/2020/day15.txt"; s:*|i;
 :,i:i!!#i
+8 11 0 19 1 2!(,0;,1;,2;,3;,4;,5)
 {x 1}@(2020-#i)({l:z-1; n:l-l^x@y; (x,(,y)!l;n;z+1)}.)/(i;s;#i)
447
```

&hellip;except this doesn't actually work. The k implementation that I'm using implements its dicts not as "proper" hash maps, but as two arrays, so dict lookup is actually just `X?y` in disguise. This is quite unfortunate, but at least for this problem that doesn't stop us, as the task is rigid enough for us to hard-code some numbers. If we just have some array `m` such that `m[i]` yields the last time that `i` was said, we effectively have a dict. The numbers can never grow larger than the number of steps, so there's a good cap to it as well.

```k
 i:.'","\1:"./aoc/2020/day15.txt"; s:*|i;
 m:30000000#0N; m[i]:1+!#i;
 11721679= ,*(30000000-#i)({l:y-1; n:l-l^m[x]; m[x]:l; (n;y+1)}.)/(s;1+#i)
```

This is still mighty slow, clocking in at around 14 seconds on my machine. Optimising this might be fun, although I suppose one would have to resort to bit-level hacking, which I'm not sure k is particularly well equipped to do (nor optimised for).

---

Full solution:

```k
 i:.'","\1:"./aoc/2020/day15.txt"; s:*|i
 447= *(2020-#i){(1+-1^(1_x)?*x),x}/|i
1
 m:30000000#0N; m[i]:!#i;
 11721679= *(30000000-#i)({l:y-1; n:l-l^m[x]; m[x]:l; (n;y+1)}.)/(s;#i)
1
```

[^1]: These are the first few words I write, and I'm deliberately not editing them later. This means you are now able to judge how I did.

[^2]: Which I'm always glad to hear about!

[^3]: Yes, that is a forward slash *followed by a space*. More on this insanity later.

[^4]: Another use for `:` is TODO

[^5]: Did I mention just how *insane* this language is? Maybe that's why it's so fun :)

[^6]: Several more mainstream languages that are inspired by array languages, like numpy, julia, or R, call this "broadcasting".

[^7]: It's times like these that I'm so glad that BQN has this weird case-based syntax for syntactic classes. Makes things much clearer after the initial acclimation phase.

[^8]: Note that K does not go so hard in the direction of combinators, as BQN or APL might. You won't find 3-trains like `w(FGH)x ≡ (wFx)G(wHx)` here.

[^9]: Notice in particular that the colon on the very right is *not* indicating a monadic use, but is instead part of *each left*.

[^10]: One could technically also just define them—at least `ngn/k` allows one to define new verbs as Unicode symbols, but I feel like this sort of goes against the spirit of the language, so I won't make much use of this functionality, if only so I don't immediately turn this language into a weird version of BQN.

    ```k
         (≤):{(x=y)|x<y}      / or x<y+1
         (!9) ≤ 1,(!4),5,6+!3
        1 0 0 0 0 1 1 1 1
    ```

[^11]: I wonder what the carbon footprint is for this journey.

[^12]: I'm surprised that there's no joke about multiple realities at this point.
