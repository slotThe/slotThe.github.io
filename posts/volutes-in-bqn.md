---
title: Integer Volutes in BQN
date: 2024-11-08
last-modified: 2024-11-13
tags: BQN, array-lang
og-description: A rendition of Joey Tuttle's algorithm for generating integer volutes.
---

On [day 3](https://tony-zorman.com/posts/aoc-in-bqn.html#day-3)
of my post about doing some
[Advent of Code problems in BQN](https://tony-zorman.com/posts/aoc-in-bqn.html),
the task was to walk along some *integer volutes*:
spirals, either spiralling inwards—involutes—or outwards—evolutes—in either direction.
While the specific problem didn't require it,
I think it's a pretty neat exercise to think about how to generate these things.
Let's talk about that.

<!--more-->

For example, the following is a left-spiraling evolute.

``` bqn
    Evolute 9
┌─
╵ 64 63 62 61 60 59 58 57 56
  65 36 35 34 33 32 31 30 55
  66 37 16 15 14 13 12 29 54
  67 38 17  4  3  2 11 28 53
  68 39 18  5  0  1 10 27 52
  69 40 19  6  7  8  9 26 51
  70 41 20 21 22 23 24 25 50
  71 42 43 44 45 46 47 48 49
  72 73 74 75 76 77 78 79 80
                             ┘
```

Quite pretty, isn't it?

Before we start, a big disclaimer:
this is neither my work, nor did I come up with any of the novel insights presented here.
Rather, there is an article by Eugene McDonnell (available
[here](https://code.jsoftware.com/wiki/Doc/Articles/Play132)
or [here](https://www.jsoftware.com/papers/play132.htm))
who talks about a solution to this problem by Joey Tuttle.[^8]
Both of the links above are not entirely satisfactory digitisations of the original article,
so in writing this post I'm trying to contribute a perhaps more palatable version.[^11]

This post is written in the REPL-like fashion that I've already used above:
input is indented by four spaces,
and output is flush to the left.
As such, there will be lots of examples that highlight certain aspects of the current sub-problem to be solved.

Finally, the article probably won't make much sense unless you know a little bit of BQN already,
the basics of which are probably best learned from the
[official website](https://mlochbaum.github.io/BQN/).

# The coordinate based solution

Let's initially produce a coordinate-based solution.
Our first sub-goal is to generate the correct indices in order.
Put the origin `⟨0 0⟩` into the middle, at the number `0`.
Our coordinate system will be that of indexing into the array; i.e.,
`0‿1`, `¯1‿0`, `0‿¯1`, and `1‿0` represent east, north, west, and south, respectively.
Looking at the 9×9 spiral above,
let's think about what kind of steps we're taking:

```
0  -E->  1  -N->  2  -W->  3  -W->  4  -S->  5  -S->  6  -E->  7  -E->  8 …
```

Just writing out the directions yields something like `ENWWSSEEENNNWWWWSSSS…`.
We alternate between east, north, west, and south in that order,
and always bunch up two of them with the same frequency—first one, then two, etc.
In particular, we "break into" a new iteration in the spiralling
every time a number is increased:

```
0     0 1       2     4 3 2     4 3 2     4 3 2
              0 1       0 1     5 0 1     5 0 1
                                6         6 7 8 9
```

If we want to end up with a square,
our sequence would look like \(1, 1, 2, 2, … k-1, k-1, k, k, k\);
repeating the last number three times instead of continuing with \(k+1\).

All in all, we need to generate two sequences of numbers:
the coordinates, and their associated frequencies.
Given a grid of size \(n\), some quick maths tells us that
we have to generate a sequence with \(2n - 1\) directions.[^7]

``` bqn
    { (1-˜𝕩×2)⥊⟨0‿1,¯1‿0,0‿¯1,1‿0⟩ } 3 # Generate this many indices
⟨ ⟨ 0 1 ⟩ ⟨ ¯1 0 ⟩ ⟨ 0 ¯1 ⟩ ⟨ 1 0 ⟩ ⟨ 0 1 ⟩ ⟩
    {(𝕩-1)∾˜2/1+↕𝕩-1} 3 # Double with an extra bit at the end
⟨ 1 1 2 2 2 ⟩
```

For doubling a given vector of numbers,
we can conveniently use
*replicate* ([`/`](https://mlochbaum.github.io/BQN/doc/replicate.html)),
and then manually add the number that should be repeated three times to the end.

``` bqn
    { 𝕊n: # Smush everything together.
      ((n-1)∾˜2/1+↕n-1){<˘𝕨‿2⥊𝕩}¨(1-˜n×2)⥊⟨0‿1,¯1‿0,0‿¯1,1‿0⟩
    } 3
┌─
· ⟨ ⟨ 0 1 ⟩ ⟩ ⟨ ⟨ ¯1 0 ⟩ ⟩ ⟨ ⟨ 0 ¯1 ⟩ ⟨ 0 ¯1 ⟩ ⟩ ⟨ ⟨ 1 0 ⟩ ⟨ 1 0 ⟩ ⟩ ⟨ ⟨ 0 1 ⟩ ⟨ 0 1 ⟩ ⟩
                                                                                         ┘
    { 𝕊n: # Flatten.
      ∾((n-1)∾˜2/1+↕n-1){<˘𝕨‿2⥊𝕩}¨(1-˜n×2)⥊⟨0‿1,¯1‿0,0‿¯1,1‿0⟩
    } 3
⟨ ⟨ 0 1 ⟩ ⟨ ¯1 0 ⟩ ⟨ 0 ¯1 ⟩ ⟨ 0 ¯1 ⟩ ⟨ 1 0 ⟩ ⟨ 1 0 ⟩ ⟨ 0 1 ⟩ ⟨ 0 ⟩ ⟩
```

Smushing everything together works by
*reshaping* ([`⥊`](https://mlochbaum.github.io/BQN/doc/reshape.html))
the vector of length two into an array with two columns,
and as many rows as the desired frequency.
Flattening everything is done by calling
*enclose* ([`<`](https://mlochbaum.github.io/BQN/doc/enclose.html))
on each major cell.

``` bqn
    3 {𝕨‿2⥊𝕩} ¯1‿0
┌─
╵ ¯1 0
  ¯1 0
  ¯1 0
       ┘
    3 {<˘𝕨‿2⥊𝕩} ¯1‿0
⟨ ⟨ ¯1 0 ⟩ ⟨ ¯1 0 ⟩ ⟨ ¯1 0 ⟩ ⟩
```

This yields the function to generate a vector of directions.

``` bqn
    Dirs ← { 𝕊n: # Directions for an n×n grid
      ∾((𝕩-1)∾˜2/1+↕𝕩-1){<˘𝕨‿2⥊𝕩}¨(1-˜n×2)⥊⟨0‿1,¯1‿0,0‿¯1,1‿0⟩
    }
(function block)
```

To get the coordinates, we just have to "walk the path"—that is,
compute the
`+`-*scan* ([`` ` ``](https://mlochbaum.github.io/BQN/doc/scan.html))
and add the origin.

``` bqn
    Coords ← { (<0‿0)∾ +` Dirs𝕩 }
(function block)
    Coords 3
⟨ ⟨ 0 0 ⟩ ⟨ 0 1 ⟩ ⟨ ¯1 1 ⟩ ⟨ ¯1 0 ⟩ ⟨ ¯1 ¯1 ⟩ ⟨ 0 ¯1 ⟩ ⟨ 1 ¯1 ⟩ ⟨ 1 0 ⟩ ⟨ 1 1 ⟩ ⟩
```

These coordinates are associated to the numbers that their respective index represents:
`0` for `⟨0 0⟩`, `1` for `⟨0 1⟩`, and so on.

``` bqn
    (↕9) ≍ Coords 3
┌─
╵ 0       1       2        3        4         5        6        7       8
  ⟨ 0 0 ⟩ ⟨ 0 1 ⟩ ⟨ ¯1 1 ⟩ ⟨ ¯1 0 ⟩ ⟨ ¯1 ¯1 ⟩ ⟨ 0 ¯1 ⟩ ⟨ 1 ¯1 ⟩ ⟨ 1 0 ⟩ ⟨ 1 1 ⟩
                                                                                ┘
```

All that's left to do is to *reshape* the argument into the correct form.
This uses the fact that coordinates have a nice lexicographic ordering.

``` bqn
    ↕3‿3          # Origin top left
┌─
╵ ⟨ 0 0 ⟩ ⟨ 0 1 ⟩ ⟨ 0 2 ⟩
  ⟨ 1 0 ⟩ ⟨ 1 1 ⟩ ⟨ 1 2 ⟩
  ⟨ 2 0 ⟩ ⟨ 2 1 ⟩ ⟨ 2 2 ⟩
                          ┘
    (↕3‿3) - <1‿1 # Origin in the middle, where we want it
┌─
╵ ⟨ ¯1 ¯1 ⟩ ⟨ ¯1 0 ⟩ ⟨ ¯1 1 ⟩
  ⟨ 0 ¯1 ⟩  ⟨ 0 0 ⟩  ⟨ 0 1 ⟩
  ⟨ 1 ¯1 ⟩  ⟨ 1 0 ⟩  ⟨ 1 1 ⟩
                              ┘
```

We first order the coordinates by their columns (the first number), and then by their rows (the second).
A verbose way of doing that would be the following:[^9]

``` bqn
    (Coords 3) ⋈¨ ↕9        # Zip coordinates to indices
┌─
· ⟨ ⟨ 0 0 ⟩ 0 ⟩ ⟨ ⟨ 0 1 ⟩ 1 ⟩ ⟨ ⟨ ¯1 1 ⟩ 2 ⟩ ⟨ ⟨ ¯1 0 ⟩ 3 ⟩ ⟨ ⟨ ¯1 ¯1 ⟩ 4 ⟩
  ⟨ ⟨ 0 ¯1 ⟩ 5 ⟩ ⟨ ⟨ 1 ¯1 ⟩ 6 ⟩ ⟨ ⟨ 1 0 ⟩ 7 ⟩ ⟨ ⟨ 1 1 ⟩ 8 ⟩
                                                                            ┘
    ∧ (Coords 3) ⋈¨ ↕9      # Sort
┌─
· ⟨ ⟨ ¯1 ¯1 ⟩ 4 ⟩ ⟨ ⟨ ¯1 0 ⟩ 3 ⟩ ⟨ ⟨ ¯1 1 ⟩ 2 ⟩ ⟨ ⟨ 0 ¯1 ⟩ 5 ⟩ ⟨ ⟨ 0 0 ⟩ 0 ⟩
  ⟨ ⟨ 0 1 ⟩ 1 ⟩ ⟨ ⟨ 1 ¯1 ⟩ 6 ⟩ ⟨ ⟨ 1 0 ⟩ 7 ⟩ ⟨ ⟨ 1 1 ⟩ 8 ⟩
                                                                            ┘
    1⊑¨ ∧ (Coords 3) ⋈¨ ↕9  # Get indices
⟨ 4 3 2 5 0 1 6 7 8 ⟩
    3‿3⥊1⊑¨∧(Coords 3)⋈¨↕9  # Reshape
┌─
╵ 4 3 2
  5 0 1
  6 7 8
        ┘
```

This works, but feels a bit unsatisfactory.
We seem to be doing a lot of extra work just to get the correct ordering of the indices.
Luckily, BQN has a primitive function—two, actually—that exactly encapsulates operations like the above:
a *grade*.
Briefly,
given an array with major cells \((c_1, \dots, c_n)\),
*grade up* ([`⍋`](https://mlochbaum.github.io/BQN/doc/order.html#grade))
returns a permutation \((i_1, \dots, i_n)\) of the indices,
which yields the order that would sort the cells:
\(c_{i_1}\) is the smallest element, \(c_{i_2}\) is the second smallest, and so on.
This can be used to sort the vector by *picking* each \(i_j\) in order.[^3]

``` bqn
    a ← ⟨3,1,0,4,4,6,7,19,16⟩
⟨ 3 1 0 4 4 6 7 19 16 ⟩
    g ← ⍋a
⟨ 2 1 0 3 4 5 6 8 7 ⟩
    (⊑g)⊑a    # smallest element
0
    (¯1⊑g)⊑a  # Largest element
19
    g⊏a       # Sort: g is a vector of indices into a
⟨ 0 1 3 4 4 6 7 16 19 ⟩
    # Grading is the same as sorting and getting the indices
    (⍋a) ≡ 1⊑¨∧a⋈¨↕9
1
```

Putting everything together, we can write the first version of `Evolute`.

``` bqn
    Evolute ← { 𝕩‿𝕩⥊ ⍋ Coords𝕩 }
(function block)
    Evolute 3
┌─
╵ 4 3 2
  5 0 1
  6 7 8
        ┘
    Evolute 5
┌─
╵ 16 15 14 13 12
  17  4  3  2 11
  18  5  0  1 10
  19  6  7  8  9
  20 21 22 23 24
                 ┘
```

Nice.

# A flat solution

While I think `Evolute` is quite neat,
a fun aspect of array programming is to come up with
[completely](https://saltysylvi.github.io/blog/flat1.html)
[flat](https://saltysylvi.github.io/blog/flat2.html)
solutions to problems.
That is, solutions that do not use any nested arrays—like vectors of coordinates—whatsoever.
This is often much more efficient,
and may yield new insights in how the problem actually works.
Plus, working in a more constrained setting can be fun just in and of itself.

Let's start with the flat version of the completed spiral.

``` bqn
    spiral ← ⥊Evolute 3
⟨ 4 3 2 5 0 1 6 7 8 ⟩
```

Naively, the general strategy is this:
throw transformations `fᵢ` at `spiral`
until we arrive at some vector `v` that is reasonably easy to generate;
i.e., `v ≡ f₁ … fₙ spiral`.
Then, invert all of the `fᵢ` to re-generate the original vector:
`spiral ≡ fₙ⁻¹ … f₁⁻¹ v`.[^4]
As far as I can tell, one just needs enough intuition about these flat arrays to guess nice enough functions.

Now, from the first solution we already know that `spiral` is the *grade* of some list of coordinates,
which we however have no hope of recovering.
After enough staring, one notices that `spiral` itself looks like a permutation<!--
-->—why not try to find the inverse of that?
Thankfully this is very easy, as *grade up* is self-inverse in this case:[^5]

``` bqn
    ⍋⍋spiral
⟨ 4 3 2 5 0 1 6 7 8 ⟩
    spiral≡⍋⍋spiral
1
    ⍋spiral # This is the permutation we want.
⟨ 4 5 2 1 0 3 6 7 8 ⟩
```

I think that `⍋spiral` doesn't look any less random than `spiral`,
but Tuttle evidently saw something,
and instead checked whether this list was the running sum of a good one;
i.e., whether there exists some nice `y` such that ``(⍋spiral) ≡ +`y``.
This in particular means that we have to compute the inverse of ``+` ``:
subtract each element from its predecessor, or `-⟜»` for short.

``` bqn
    -⟜» ⍋spiral
⟨ 4 1 ¯3 ¯1 ¯1 3 3 1 1 ⟩
    +` -⟜» ⍋spiral
⟨ 4 5 2 1 0 3 6 7 8 ⟩
    (⍋spiral) ≡ (+`-⟜»⍋spiral)
1
```

In fact, we can even ask BQN to do this for us by means of
*undo* ([`⁼`](https://mlochbaum.github.io/BQN/doc/undo.html)).

``` bqn
    +`⁼ ⍋spiral
⟨ 4 1 ¯3 ¯1 ¯1 3 3 1 1 ⟩
```

Finally, this looks like something we can generate!
Let's take a look at a 5×5 grid, so the structure becomes even more obvious:

``` bqn
    +`⁼⍋⥊Evolute 5
⟨ 12 1 ¯5 ¯1 ¯1 5 5 1 1 1 ¯5 ¯5 ¯5 ¯1 ¯1 ¯1 ¯1 5 5 5 5 1 1 1 1 ⟩
```

Ignoring the number at the start, we have an alternation of `1 ¯n ¯1 n`, where `n` is the size of the evolute.
Disregarding multiple occurrences and the first number,
there are exactly `(2×n)-1` "primitive" numbers
to which we need to associate frequencies,
which matches with the coordinate generation from before.

``` bqn
    { (1-˜2×𝕩)⥊⟨1,-𝕩,¯1,𝕩⟩ } 3
⟨ 1 ¯3 ¯1 3 1 ⟩
    { (1-˜2×𝕩)⥊⟨1,-𝕩,¯1,𝕩⟩ } 5
⟨ 1 ¯5 ¯1 5 1 ¯5 ¯1 5 1 ⟩
```

These numbers grow in their frequency in blocks of two; first one of each, then two, and so on.
Everything here is entirely analogous to the coordinate case,
including the fact that we need to repeat the last number three times in order to obtain a square grid.

``` bqn
    { 2/1+↕𝕩-1 } 5
⟨ 1 1 2 2 3 3 4 4 ⟩
    { (𝕩-1)∾˜2/1+↕𝕩-1 } 5 # Need correcting term at the end
⟨ 1 1 2 2 3 3 4 4 4 ⟩
    { ((𝕩-1)∾˜2/1+↕𝕩-1)/(1-˜2×𝕩)⥊⟨1,-𝕩,¯1,𝕩⟩ } 3
⟨ 1 ¯3 ¯1 ¯1 3 3 1 1 ⟩
```

As for the first number that we ignored so far: it's not actually important!
What we do with this list is a `+`-*scan*, followed by a *grade up* to sort the indices.
This means that only the relative value of the *scan* is important for the following *grade*,
so outside of special cases like `∞`, the first number could be almost anything.

``` bqn
    { 0∾((𝕩-1)∾˜2/1+↕𝕩-1)/(1-˜2×𝕩)⥊⟨1,-𝕩,¯1,𝕩⟩ } 5
⟨ 0 1 ¯5 ¯1 ¯1 5 5 1 1 1 ¯5 ¯5 ¯5 ¯1 ¯1 ¯1 ¯1 5 5 5 5 1 1 1 1 ⟩
```

In particular, this opens up a nice shortcut.
Instead of manually adding `𝕩-1` to the back and `0` to the front,
we can just add `𝕩` itself to the back,
and then *nudge* the whole array to the right,
inserting the filler `0` as the first element.

``` bqn
    { 0∾((𝕩-1)∾˜2/1+↕𝕩-1) } 5
⟨ 0 1 1 2 2 3 3 4 4 4 ⟩
    { »(𝕩∾˜2/1+↕𝕩-1) } 5
⟨ 0 1 1 2 2 3 3 4 4 ⟩
    { »(¯1↓2/1+↕𝕩) } 5
⟨ 0 1 1 2 2 3 3 4 4 ⟩
    { »(¯1↓2/1+↕𝕩)/(1-˜2×𝕩)⥊⟨1,-𝕩,¯1,𝕩⟩ } 5
⟨ 0 1 ¯5 ¯1 ¯1 5 5 1 1 1 ¯5 ¯5 ¯5 ¯1 ¯1 ¯1 ¯1 5 5 5 5 1 1 1 1 ⟩
```

We obtain a completely flat version of `Evolute`,
which works just like the original function.

``` bqn
    EvoluteFlat ← { 𝕩‿𝕩⥊⍋+`»(¯1↓2/1+↕𝕩)/(1-˜2×𝕩)⥊⟨1,-𝕩,¯1,𝕩⟩ }
(function block)
    EvoluteFlat 3
┌─
╵ 4 3 2
  5 0 1
  6 7 8
        ┘
    EvoluteFlat 5
┌─
╵ 16 15 14 13 12
  17  4  3  2 11
  18  5  0  1 10
  19  6  7  8  9
  20 21 22 23 24
                 ┘
```

Getting a rightwards spiral just involves
twiddling with the vector `⟨1,-𝕩,¯1,𝕩⟩` a little bit.
I'll leave that as an exercise for the interested reader.

# Conclusion

Here are complete definitions of both solutions.

``` bqn
E1 ← {𝕩‿𝕩⥊⍋(<0‿0)∾+`∾((2/1+↕𝕩-2)∾3⥊𝕩-1){<˘𝕨‿2⥊𝕩}¨(1-˜𝕩×2)⥊⟨0‿1,¯1‿0,0‿¯1,1‿0⟩}
E2 ← {𝕩‿𝕩⥊⍋+`»(¯1↓2/1+↕𝕩)/(1-˜2×𝕩)⥊⟨1,-𝕩,¯1,𝕩⟩}
```

I certainly think that `E2` is much prettier than `E1`;
not only because it's shorter, but also because there seems to be less wrangling to get everything to line up correctly.[^12]
Seeing these side by side does reveal that they use a lot of the same techniques,
however, which I think is interesting.

---

As a bonus, here is the mandatory golfed solution,
this time as an involute.

``` bqn
    G ← {(⊒˜∾⍉∘⌽+≠)⍟2⍟𝕩↕0‿0}
(function block)
    SLG ← {(↕∘≠∾⍉∘⌽+≠)⍟(2×𝕩)↕0‿0} # Slighly less golfed
(function block)
    G 5
┌─
╵  0  1  2  3 4
  15 16 17 18 5
  14 23 24 19 6
  13 22 21 20 7
  12 11 10  9 8
                ┘
    (SLG 5) ≡ G 5
1
```

I will leave it to the interested reader to figure out that one—or to consult the
[relevant StackExchange thread](https://codegolf.stackexchange.com/questions/241803/print-a-nxn-integer-involute),
which I perhaps should have looked up before writing this whole thing.
Still, all of this transposing and reversing the matrix means that this solution is on the slower side.
Here's a quick an dirty comparison:

``` bqn
    )time G   1000  # Golfed version
1.0114s
    )time G   2000
13.885s
    )time SLG 1000  # Slightly less golfed version
651.3ms
    )time SLG 2000
8.1603s
    )time E1  1000  # Using indices
218ms
    )time E1  2000
950.2ms
    )time E2  1000  # Flat
4.197ms
    )time E2  2000
12.98ms
```

While both are cubic and thus degrade fast,
`SLG` is faster than `G` because `↕≠` to generate a list of integers is much quicker than some *progressive index of* shenanigans to do the same thing.
At the end of the day, I think I prefer the flat solution :)

[^3]: In fact, in APL this is the only way to do this—there is no other sorting primitive!

[^4]: Note that, in particular, we are interested in left inverses of all the `fᵢ`.
      This is a bit in opposition to the facilities that BQN provides by default, like
      *undo* to find the right inverse.
      However, we'll manage.

[^5]: A quick justification:
      suppose that we have a vector \(n \defeq (n_1, \dots, n_k)\) of natural numbers,
      such that every number from \(0\) to \(k-1\) appears exactly once.
      Using *grade up*, we obtain a permutation \(\sigma \defeq (i_1, \dots, i_k)\).
      If one *grades* that permutation in the same direction,
      then whatever index \(i_j\) has value \(0\)
      will be the very first element of the new permutation \(\tau \defeq (i_j, \dots, i_\ell)\),
      and likewise for the other numbers.
      Thus, \(\tau \sigma (n) = n\).

      Of course, `⍋⍋` it not the identity in general, but it is a useful operation nonetheless:
      it "ranks" the elements of the input,
      with `0` corresponding to the smallest element, and so on.
      This is known as the
      [ordinal idiom](https://mlochbaum.github.io/BQN/doc/order.html#ordinals).

[^7]: The frequencies have to add up to \(n^2 - 1\), covering all points but the origin.
      A given sequence \(1, 1, 2, 2, \dots, k-1, k-1, k, k, k\) has \(2k + 1\) members
      and sums up to \(k^2 + 2k\).
      Solving \(k^2 + 2k = n^2 - 1\) for \(k\) has sensible roots,
      \(\pm n - 1\), of which we of course pick \(n-1\).
      Substituting \(k=n-1\),
      we have to generate \(2n - 1\) frequencies,
      the values of which indeed sum up to \((n-1)^2 + 2(n-1) = n^2 - 1\).

[^8]: Special thanks to Marshall Lochbaum for providing these links.

[^9]: {-} Line breaks in the output for clarity.

[^11]: Plus, I don't know any J, so reading the article in detail is a lot more work than just writing it in BQN myself.

[^12]: Looking at [BQNcrate](https://mlochbaum.github.io/bqncrate/),
       one could even shorten `E2` a little further:

       ``` bqn
       E3 ← {𝕩‿𝕩⥊⍋+`»(¯1↓2/1+↕𝕩)(⊣/≠⊸⥊)⟨1,-𝕩,¯1,𝕩⟩}
       ```

       This *reshapes* the list `⟨1,-𝕩,¯1,𝕩⟩` according to the length of `¯1↓2/1+↕𝕩`,
       without us having to explicitly specify that it will be `1-˜2×𝕩`.
       I think, however, that arriving at this number yields some insights that one would otherwise not have had,
       so I prefer leaving it in the final solution.
