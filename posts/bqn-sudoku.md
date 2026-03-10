---
title: Sudoku Solving in BQN
date: 2026-02-27
last-modified: 2026-02-27
tags: array-lang, BQN
og-description: From a simple DFS approach to something truly array oriented (and pretty).
---

A few days ago, I read a very interesting [blog post](https://blog.veitheller.de/Simple_Sudoku_Solvers_SII,_EI:_Dyalog_APL.html) by Veit Heller about solving a regular 9×9 Sudoku puzzle in Dyalog APL.[^1] It's part of a series of blog posts, starting with a [reference implementation](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_I:_Python_(Reference).html) in Python, about writing a simple depth first search Sudoku solver in a variety of different languages. This made me realise that I've never written such a solver—not even a stupid one—myself!

<!--more-->

Needless to say, it's probably about time for that to change. Naturally, I'll use [BQN](https://mlochbaum.github.io/BQN/) for this—what better tool could there be to solve a problem about a literal array of numbers than an array programming language.[^2]


# Setup

As mentioned above, the algorithm that the reference implementation uses—which I'll try to follow in version 0, for comparison's sake—is a straightforward depth first search. In so many words, it runs about as follows:

-   Make changes for which only one candidate exists ("propagate").
    -   If no changes are possible, the Sudoku is unsolvable.
-   If there are no unfilled spots left, return the (first) solution.
-   Find a cell with the minimal number of candidates.
    -   Create a new board for each candidate (with its value at the cell) and recurse.
    -   Return solution if any of those recursive steps solved the board.

There are quite a lot of conditionals here, which perhaps makes this solution a bit unergonomic for array languages, but let's put that aside for now and just implement the algorithm essentially verbatim.


# Version 0

You can read this post as a sort of literate program: things we've defined in previous blocks will be available in later ones.[^3] Let's first define a board that we'll use throughout.

```bqn
board ← [ 6‿0‿2‿1‿3‿0‿0‿0‿0
        , 0‿0‿0‿0‿0‿0‿2‿0‿4
        , 0‿0‿0‿0‿0‿0‿0‿9‿0
        , 0‿7‿0‿0‿2‿0‿0‿8‿0
        , 0‿0‿0‿6‿7‿0‿0‿3‿0
        , 3‿0‿9‿0‿0‿0‿6‿0‿0
        , 0‿0‿3‿0‿0‿0‿0‿0‿0
        , 0‿0‿0‿0‿6‿0‿8‿1‿0
        , 1‿9‿0‿0‿0‿7‿0‿0‿0 ]
```

The first thing we need to implement is some kind of `candidates` function, which should return a list of valid numbers to be inserted at a given position. If we already have a position in mind, it's pretty easy to get the correct row and column:[^4]

```bqn
   0⊏board  # first row
⟨ 6 0 2 1 3 0 0 0 0 ⟩
   1⊏⍉board # second column
⟨ 0 0 0 7 0 0 0 0 9 ⟩
```

Next up is to find a way to locate the 3×3 grid a given cell is in, and get all numbers from that region. So, for example given our board

```
┌─
╵ 6 0 2 1 3 0 0 0 0
  0 0 0 0 0 0 2 0 4
  0 0 0 0 0 0 0 9 0
  0 7 0 0 2 0 0 8 0
  0 0 0 6 7 0 0 3 0
  3 0 9 0 0 0 6 0 0
  0 0 3 0 0 0 0 0 0
  0 0 0 0 6 0 8 1 0
  1 9 0 0 0 7 0 0 0
                    ┘
```

if we want to zoom in to the `0` at index `1‿2`, we would have to consider the following numbers as "not available":

```
┌─
╵ 6 0 2 · · · · · ·
  0 0 0 0 0 0 2 0 4
  0 0 0 · · · · · ·
  · · 0 · · · · · ·
  · · 0 · · · · · ·
  · · 9 · · · · · ·
  · · 3 · · · · · ·
  · · 0 · · · · · ·
  · · 0 · · · · · ·
                    ┘
```

Let's create a matrix 3×3 matrix where the element at index `i‿j` exactly corresponds to the neighbouring grid indices:[^5]

```bqn
   ⌊3÷˜↕9
⟨ 0 0 0 1 1 1 2 2 2 ⟩
   2⥊<⌊3÷˜↕9
⟨ ⟨ 0 0 0 1 1 1 2 2 2 ⟩ ⟨ 0 0 0 1 1 1 2 2 2 ⟩ ⟩
   (2⥊<⌊3÷˜↕9)⊔↕9‿9
┌─
╵ ┌─                          ┌─                          ┌─
  ╵ ⟨ 0 0 ⟩ ⟨ 0 1 ⟩ ⟨ 0 2 ⟩   ╵ ⟨ 0 3 ⟩ ⟨ 0 4 ⟩ ⟨ 0 5 ⟩   ╵ ⟨ 0 6 ⟩ ⟨ 0 7 ⟩ ⟨ 0 8 ⟩
    ⟨ 1 0 ⟩ ⟨ 1 1 ⟩ ⟨ 1 2 ⟩     ⟨ 1 3 ⟩ ⟨ 1 4 ⟩ ⟨ 1 5 ⟩     ⟨ 1 6 ⟩ ⟨ 1 7 ⟩ ⟨ 1 8 ⟩
    ⟨ 2 0 ⟩ ⟨ 2 1 ⟩ ⟨ 2 2 ⟩     ⟨ 2 3 ⟩ ⟨ 2 4 ⟩ ⟨ 2 5 ⟩     ⟨ 2 6 ⟩ ⟨ 2 7 ⟩ ⟨ 2 8 ⟩
                            ┘                           ┘                           ┘
  ┌─                          ┌─                          ┌─
  ╵ ⟨ 3 0 ⟩ ⟨ 3 1 ⟩ ⟨ 3 2 ⟩   ╵ ⟨ 3 3 ⟩ ⟨ 3 4 ⟩ ⟨ 3 5 ⟩   ╵ ⟨ 3 6 ⟩ ⟨ 3 7 ⟩ ⟨ 3 8 ⟩
    ⟨ 4 0 ⟩ ⟨ 4 1 ⟩ ⟨ 4 2 ⟩     ⟨ 4 3 ⟩ ⟨ 4 4 ⟩ ⟨ 4 5 ⟩     ⟨ 4 6 ⟩ ⟨ 4 7 ⟩ ⟨ 4 8 ⟩
    ⟨ 5 0 ⟩ ⟨ 5 1 ⟩ ⟨ 5 2 ⟩     ⟨ 5 3 ⟩ ⟨ 5 4 ⟩ ⟨ 5 5 ⟩     ⟨ 5 6 ⟩ ⟨ 5 7 ⟩ ⟨ 5 8 ⟩
                            ┘                           ┘                           ┘
  ┌─                          ┌─                          ┌─
  ╵ ⟨ 6 0 ⟩ ⟨ 6 1 ⟩ ⟨ 6 2 ⟩   ╵ ⟨ 6 3 ⟩ ⟨ 6 4 ⟩ ⟨ 6 5 ⟩   ╵ ⟨ 6 6 ⟩ ⟨ 6 7 ⟩ ⟨ 6 8 ⟩
    ⟨ 7 0 ⟩ ⟨ 7 1 ⟩ ⟨ 7 2 ⟩     ⟨ 7 3 ⟩ ⟨ 7 4 ⟩ ⟨ 7 5 ⟩     ⟨ 7 6 ⟩ ⟨ 7 7 ⟩ ⟨ 7 8 ⟩
    ⟨ 8 0 ⟩ ⟨ 8 1 ⟩ ⟨ 8 2 ⟩     ⟨ 8 3 ⟩ ⟨ 8 4 ⟩ ⟨ 8 5 ⟩     ⟨ 8 6 ⟩ ⟨ 8 7 ⟩ ⟨ 8 8 ⟩
                            ┘                           ┘                           ┘
                                                                                      ┘
   3↑¨ nbs ← ⥊¨(2⥊<⌊3÷˜↕9)⊔↕9‿9 # neighbours as flat coordinates
┌─
╵ ⟨ ⟨ 0 0 ⟩ ⟨ 0 1 ⟩ ⟨ 0 2 ⟩ ⟩ ⟨ ⟨ 0 3 ⟩ ⟨ 0 4 ⟩ ⟨ 0 5 ⟩ ⟩ ⟨ ⟨ 0 6 ⟩ ⟨ 0 7 ⟩ ⟨ 0 8 ⟩ ⟩
  ⟨ ⟨ 3 0 ⟩ ⟨ 3 1 ⟩ ⟨ 3 2 ⟩ ⟩ ⟨ ⟨ 3 3 ⟩ ⟨ 3 4 ⟩ ⟨ 3 5 ⟩ ⟩ ⟨ ⟨ 3 6 ⟩ ⟨ 3 7 ⟩ ⟨ 3 8 ⟩ ⟩
  ⟨ ⟨ 6 0 ⟩ ⟨ 6 1 ⟩ ⟨ 6 2 ⟩ ⟩ ⟨ ⟨ 6 3 ⟩ ⟨ 6 4 ⟩ ⟨ 6 5 ⟩ ⟩ ⟨ ⟨ 6 6 ⟩ ⟨ 6 7 ⟩ ⟨ 6 8 ⟩ ⟩
                                                                                      ┘
```

The above uses the [multidimensional grouping](https://mlochbaum.github.io/BQN/doc/group.html#multidimensional-grouping) functionality of *group*: the first list runs along the rows of the matrix, and the second one runs along the columns.

To translate a given coordinate `i‿j` of a Sudoku grid into one that indexes into `nbs`, divide each number by 3 and round down.

```bqn
   ⌊3÷˜1‿2 # upper left
⟨ 0 0 ⟩
   ⌊3÷˜5‿3 # middle
⟨ 1 1 ⟩
   ⌊3÷˜7‿8 # bottom right
⟨ 2 2 ⟩
   3‿3⥊ board⊑˜ nbs⊑˜ ⌊3÷˜1‿2
┌─
╵ 6 0 2
  0 0 0
  0 0 0
        ┘
```

Now having access to the numbers we *don't* want, we just have to "negate" the aggregated array somehow. This kind of pruning can be done in various ways, but I find APL's *without* function `~` to be particularly neat. It's essentially a `\setminus`, although it works on arrays, and can be implemented in BQN via the *train* `¬∘∊/⊣`:

```bqn
   ↕10
⟨ 0 1 2 3 4 5 6 7 8 9 ⟩
   (↕10) (¬∘∊/⊣) 5‿6‿1‿0 # ↕10 without 5, 6, 1, or 0
⟨ 2 3 4 7 8 9 ⟩
   0‿1 { i‿j 𝕊 b: (↕10) (¬∘∊/⊣) (j⊏⍉b)∾(i⊏b) } board
⟨ 4 5 8 ⟩
```

Amalgamating all of these tricks, we can write our candidates function:

```bqn
Cands ← { i‿j𝕊b: (↕10) (¬∘∊/⊣) (j⊏⍉b)∾(i⊏b)∾(b⊑˜nbs⊑˜⌊3÷˜i‿j) }
```

```bqn
   1‿2 Cands board
⟨ 1 5 7 8 ⟩
   5‿6 Cands board
⟨ 1 4 5 7 ⟩
```

---

Instead of all cells, we actually only care about the empty ones; i.e., those that have a 0 in them. Filtering for them can be done with *replicate*, although this involves a bit of [additional ceremony](https://mlochbaum.github.io/BQN/doc/replicate.html#just-rank-1), since we're looking at a matrix instead of a flat list:

```bqn
   0=board                  # Empty spots
┌─
╵ 0 1 0 0 0 1 1 1 1
  1 1 1 1 1 1 0 1 0
  1 1 1 1 1 1 1 0 1
  1 0 1 1 0 1 1 0 1
  1 1 1 0 0 1 1 0 1
  0 1 0 1 1 1 0 1 1
  1 1 0 1 1 1 1 1 1
  1 1 1 1 0 1 0 0 1
  0 0 1 1 1 0 1 1 1
                    ┘
   ↕≢board                  # matrix of indices
┌─
╵ ⟨ 0 0 ⟩ ⟨ 0 1 ⟩ ⟨ 0 2 ⟩ ⟨ 0 3 ⟩ ⟨ 0 4 ⟩ ⟨ 0 5 ⟩ ⟨ 0 6 ⟩ ⟨ 0 7 ⟩ ⟨ 0 8 ⟩
  ⟨ 1 0 ⟩ ⟨ 1 1 ⟩ ⟨ 1 2 ⟩ ⟨ 1 3 ⟩ ⟨ 1 4 ⟩ ⟨ 1 5 ⟩ ⟨ 1 6 ⟩ ⟨ 1 7 ⟩ ⟨ 1 8 ⟩
  ⟨ 2 0 ⟩ ⟨ 2 1 ⟩ ⟨ 2 2 ⟩ ⟨ 2 3 ⟩ ⟨ 2 4 ⟩ ⟨ 2 5 ⟩ ⟨ 2 6 ⟩ ⟨ 2 7 ⟩ ⟨ 2 8 ⟩
  ⟨ 3 0 ⟩ ⟨ 3 1 ⟩ ⟨ 3 2 ⟩ ⟨ 3 3 ⟩ ⟨ 3 4 ⟩ ⟨ 3 5 ⟩ ⟨ 3 6 ⟩ ⟨ 3 7 ⟩ ⟨ 3 8 ⟩
  ⟨ 4 0 ⟩ ⟨ 4 1 ⟩ ⟨ 4 2 ⟩ ⟨ 4 3 ⟩ ⟨ 4 4 ⟩ ⟨ 4 5 ⟩ ⟨ 4 6 ⟩ ⟨ 4 7 ⟩ ⟨ 4 8 ⟩
  ⟨ 5 0 ⟩ ⟨ 5 1 ⟩ ⟨ 5 2 ⟩ ⟨ 5 3 ⟩ ⟨ 5 4 ⟩ ⟨ 5 5 ⟩ ⟨ 5 6 ⟩ ⟨ 5 7 ⟩ ⟨ 5 8 ⟩
  ⟨ 6 0 ⟩ ⟨ 6 1 ⟩ ⟨ 6 2 ⟩ ⟨ 6 3 ⟩ ⟨ 6 4 ⟩ ⟨ 6 5 ⟩ ⟨ 6 6 ⟩ ⟨ 6 7 ⟩ ⟨ 6 8 ⟩
  ⟨ 7 0 ⟩ ⟨ 7 1 ⟩ ⟨ 7 2 ⟩ ⟨ 7 3 ⟩ ⟨ 7 4 ⟩ ⟨ 7 5 ⟩ ⟨ 7 6 ⟩ ⟨ 7 7 ⟩ ⟨ 7 8 ⟩
  ⟨ 8 0 ⟩ ⟨ 8 1 ⟩ ⟨ 8 2 ⟩ ⟨ 8 3 ⟩ ⟨ 8 4 ⟩ ⟨ 8 5 ⟩ ⟨ 8 6 ⟩ ⟨ 8 7 ⟩ ⟨ 8 8 ⟩
                                                                          ┘
   5↑ (0=board)/○⥊(↕≢board) # Filter for indices of empty elements
⟨ ⟨ 0 1 ⟩ ⟨ 0 5 ⟩ ⟨ 0 6 ⟩ ⟨ 0 7 ⟩ ⟨ 0 8 ⟩ ⟩
```

```bqn
Empty ← {(0=𝕩)/○⥊(↕≢𝕩)}
```

Now we have everything we need to implement the first two lines of the algorithm:

> -   Make changes for which only one candidate exists ("propagate").
>     -   If no changes are possible, the Sudoku is unsolvable.

I'll first give you the version without recursion, as that's a bit prettier (and works essentially just as well, actually; the inner loop of the final solve function is reasonably fast):

```bqn
Prop ← {𝕩≡0?0; ix𝕊b: 2⊸⌊∘≠◶⟨0, {(⊑𝕩)⌾(ix⊸⊑)b}, b⟩ ix Cands b}´⟜Empty
```

We fold over all of the empty cells; if there is only one possible candidate to fill in, change the accumulator of the fold (initially the input board to `Prop`) to include that as a guess. If there are ever no candidates to fill in, the board gets set to `0`, which indicates impossibility of the puzzle.

```bqn
easy ← [ 0‿0‿0‿0‿2‿0‿0‿9‿5 # an easy puzzle
       , 6‿0‿0‿0‿1‿0‿7‿2‿4
       , 0‿0‿2‿6‿0‿5‿0‿1‿8
       , 0‿1‿7‿0‿6‿0‿0‿3‿9
       , 4‿0‿0‿0‿3‿9‿0‿0‿0
       , 5‿0‿3‿8‿0‿0‿4‿0‿2
       , 0‿0‿4‿0‿5‿6‿9‿0‿0
       , 0‿0‿5‿9‿0‿3‿0‿4‿0
       , 0‿3‿8‿1‿0‿7‿2‿0‿6 ]
```

```bqn
   10↑ Cands⟜easy¨Empty easy # A few with only one choice
⟨ ⟨ 1 3 7 8 ⟩ ⟨ 4 7 8 ⟩ ⟨ 1 ⟩ ⟨ 3 4 7 ⟩ ⟨ 4 8 ⟩ ⟨ 3 6 ⟩ ⟨ 5 8 9 ⟩ ⟨ 9 ⟩ ⟨ 3 ⟩ ⟨ 8 ⟩ ⟩
   easy ∾○< Prop easy        # Before and after
┌─
· ┌─                    ┌─
  ╵ 0 0 0 0 2 0 0 9 5   ╵ 3 8 1 7 2 4 6 9 5
    6 0 0 0 1 0 7 2 4     6 5 9 3 1 8 7 2 4
    0 0 2 6 0 5 0 1 8     7 4 2 6 9 5 3 1 8
    0 1 7 0 6 0 0 3 9     8 1 7 4 6 2 5 3 9
    4 0 0 0 3 9 0 0 0     4 2 6 5 3 9 8 7 1
    5 0 3 8 0 0 4 0 2     5 9 3 8 7 1 4 6 2
    0 0 4 0 5 6 9 0 0     1 7 4 2 5 6 9 8 3
    0 0 5 9 0 3 0 4 0     2 6 5 9 8 3 1 4 7
    0 3 8 1 0 7 2 0 6     9 3 8 1 4 7 2 5 6
                      ┘                     ┘
                                              ┘
```

Recursion essentially only involves adding a check if we've done anything—if yes, we try again, as perhaps the candidates for a specific field decreased and it's now uniquely solvable.

```bqn
Prop ← { c←1 # changed?
  { c↩0
    𝕩{𝕩≡0?0; ix𝕊b: 2⊸⌊∘≠◶⟨0, {c↩1⋄(⊑𝕩)⌾(ix⊸⊑)b}, b⟩ ix Cands b}´Empty 𝕩
  }•_while_{𝕊·:c} 𝕩
}
```

```bqn
   Prop easy          # Actually solves the whole board!
┌─
╵ 3 8 1 7 2 4 6 9 5
  6 5 9 3 1 8 7 2 4
  7 4 2 6 9 5 3 1 8
  8 1 7 4 6 2 5 3 9
  4 2 6 5 3 9 8 7 1
  5 9 3 8 7 1 4 6 2
  1 7 4 2 5 6 9 8 3
  2 6 5 9 8 3 1 4 7
  9 3 8 1 4 7 2 5 6
                    ┘
```

---

Onto the main loop:

> -   If there are no unfilled spots left, return the solution
> -   Find a cell with the minimal number of candidates
>     -   Create a new board for each candidate (with its value at the cell) and recurse
>     -   Return solution if any of those steps solve the board

To find the cell with the minimal number of elements, we can use an idiom similar to `⍋⊸⊏` (*grade up* *before* *select*), which acts as a kind of "sort by" function

```bqn
   r ← (7 •rand.Deal 7) ⋈¨ (7 •rand.Deal 7)
⟨ ⟨ 1 1 ⟩ ⟨ 2 2 ⟩ ⟨ 6 5 ⟩ ⟨ 5 4 ⟩ ⟨ 4 0 ⟩ ⟨ 0 6 ⟩ ⟨ 3 3 ⟩ ⟩
   (⍋    +´¨)⊸⊏ r # Sort  by sum
⟨ ⟨ 1 1 ⟩ ⟨ 2 2 ⟩ ⟨ 4 0 ⟩ ⟨ 0 6 ⟩ ⟨ 3 3 ⟩ ⟨ 5 4 ⟩ ⟨ 6 5 ⟩ ⟩
   (⍋   1⊸⊑¨)⊸⊏ r # Sort  by second
⟨ ⟨ 4 0 ⟩ ⟨ 1 1 ⟩ ⟨ 2 2 ⟩ ⟨ 3 3 ⟩ ⟨ 5 4 ⟩ ⟨ 6 5 ⟩ ⟨ 0 6 ⟩ ⟩
   (⊑∘⍋  +´¨)⊸⊑ r # First by sum
⟨ 1 1 ⟩
```

```bqn
   (⊑∘⍋ ≠∘Cands⟜board¨)⊸⊑ Empty board # First empty cell by number of candidates
⟨ 0 6 ⟩
```

We now just need to create a new board for every candidate, and recurse appropriately.

```bqn
Solve0 ← { Rec 𝕩:
  { 0≡𝕩? 0;      # Unsolvable
    ⊑¬0∊⥊𝕩? 𝕩;   # No unfilled spots left -> return
    𝕊b: pt ← (⊑∘⍋ ≠∘Cands⟜b¨)⊸⊑ Empty b
        0{0≢𝕩? 𝕩; Rec 𝕨⌾(pt⊸⊑)b}´pt Cands b
  } Prop𝕩
}
```

Voilà:

```bqn
   Solve0 easy
┌─
╵ 3 8 1 7 2 4 6 9 5
  6 5 9 3 1 8 7 2 4
  7 4 2 6 9 5 3 1 8
  8 1 7 4 6 2 5 3 9
  4 2 6 5 3 9 8 7 1
  5 9 3 8 7 1 4 6 2
  1 7 4 2 5 6 9 8 3
  2 6 5 9 8 3 1 4 7
  9 3 8 1 4 7 2 5 6
                    ┘
   Solve0 board
┌─
╵ 6 4 2 1 3 9 7 5 8
  9 3 1 7 8 5 2 6 4
  5 8 7 2 4 6 1 9 3
  4 7 6 9 2 3 5 8 1
  8 1 5 6 7 4 9 3 2
  3 2 9 5 1 8 6 4 7
  2 6 3 8 9 1 4 7 5
  7 5 4 3 6 2 8 1 9
  1 9 8 4 5 7 3 2 6
                    ┘
```

---

One last thing to add: the reference implementation sports a pretty-printing function that separates the grid into 3×3 chunks:

```python
def print_board(board):
    if not board:
        print("Unsolvable")
    print("-" * 25)
    for i, row in enumerate(board):
        if i and i % 3 == 0:
            print("-" * 25)
        print("|", end=" ")
        for j, e in enumerate(row):
            if j and j % 3 == 0:
                print("|", end=" ")
            print(e, end=" ")
        print("|", end="")
        print("")
    print("-" * 25)
```

Let's write that as well, so we can more easily compare the results. I'll use a slightly different approach for the implementation: in BQN it's much easier to add the vertical bars to the already existing matrix we have, and then slice and dice it:

```bqn
   board
┌─
╵ 6 0 2 1 3 0 0 0 0
  0 0 0 0 0 0 2 0 4
  0 0 0 0 0 0 0 9 0
  0 7 0 0 2 0 0 8 0
  0 0 0 6 7 0 0 3 0
  3 0 9 0 0 0 6 0 0
  0 0 3 0 0 0 0 0 0
  0 0 0 0 6 0 8 1 0
  1 9 0 0 0 7 0 0 0
                    ┘
   1 {(-𝕨)⌽˘'|'∾˘𝕨⌽˘𝕩} board # Insert a '|' at the given position
┌─
╵ 6 '|' 0 2 1 3 0 0 0 0
  0 '|' 0 0 0 0 0 2 0 4
  0 '|' 0 0 0 0 0 0 9 0
  0 '|' 7 0 0 2 0 0 8 0
  0 '|' 0 0 6 7 0 0 3 0
  3 '|' 0 9 0 0 0 6 0 0
  0 '|' 0 3 0 0 0 0 0 0
  0 '|' 0 0 0 6 0 8 1 0
  1 '|' 9 0 0 0 7 0 0 0
                        ┘
   seps ← (•Fmt¨board){(-𝕨)⌽˘'|'∾˘𝕨⌽˘𝕩}´3‿6‿9
┌─
╵ "6" "0" "2" '|' "1" "3" "0" '|' "0" "0" "0" '|'
  "0" "0" "0" '|' "0" "0" "0" '|' "2" "0" "4" '|'
  "0" "0" "0" '|' "0" "0" "0" '|' "0" "9" "0" '|'
  "0" "7" "0" '|' "0" "2" "0" '|' "0" "8" "0" '|'
  "0" "0" "0" '|' "6" "7" "0" '|' "0" "3" "0" '|'
  "3" "0" "9" '|' "0" "0" "0" '|' "6" "0" "0" '|'
  "0" "0" "3" '|' "0" "0" "0" '|' "0" "0" "0" '|'
  "0" "0" "0" '|' "0" "6" "0" '|' "8" "1" "0" '|'
  "1" "9" "0" '|' "0" "0" "7" '|' "0" "0" "0" '|'
                                                  ┘
   3‿1⥊ (⌊3÷˜↕9) ⊔ seps      # Vertical slicing (transposed for easier display)
┌─
╵ ┌─
  ╵ "6" "0" "2" '|' "1" "3" "0" '|' "0" "0" "0" '|'
    "0" "0" "0" '|' "0" "0" "0" '|' "2" "0" "4" '|'
    "0" "0" "0" '|' "0" "0" "0" '|' "0" "9" "0" '|'
                                                    ┘
  ┌─
  ╵ "0" "7" "0" '|' "0" "2" "0" '|' "0" "8" "0" '|'
    "0" "0" "0" '|' "6" "7" "0" '|' "0" "3" "0" '|'
    "3" "0" "9" '|' "0" "0" "0" '|' "6" "0" "0" '|'
                                                    ┘
  ┌─
  ╵ "0" "0" "3" '|' "0" "0" "0" '|' "0" "0" "0" '|'
    "0" "0" "0" '|' "0" "6" "0" '|' "8" "1" "0" '|'
    "1" "9" "0" '|' "0" "0" "7" '|' "0" "0" "0" '|'
                                                    ┘
                                                      ┘
```

The rest is just a bit of additional printing of the horizontal bars:

```bqn
PP ← { O←{𝕊·:•Out"-------------------------"} # Pretty-print board
  {O@ ⋄ •Out∘∾˘ '|'∾˘ " "⊸∾¨𝕩}¨ (⌊3÷˜↕9) ⊔ (•Fmt¨𝕩){(-𝕨)⌽˘'|'∾˘𝕨⌽˘𝕩}´⟨3,6,9⟩ ⋄ O@
}
```

```bqn
   PP board        ⋄ @
-------------------------
| 6 0 2 | 1 3 0 | 0 0 0 |
| 0 0 0 | 0 0 0 | 2 0 4 |
| 0 0 0 | 0 0 0 | 0 9 0 |
-------------------------
| 0 7 0 | 0 2 0 | 0 8 0 |
| 0 0 0 | 6 7 0 | 0 3 0 |
| 3 0 9 | 0 0 0 | 6 0 0 |
-------------------------
| 0 0 3 | 0 0 0 | 0 0 0 |
| 0 0 0 | 0 6 0 | 8 1 0 |
| 1 9 0 | 0 0 7 | 0 0 0 |
-------------------------
@
   PP Solve0 board ⋄ @
-------------------------
| 6 4 2 | 1 3 9 | 7 5 8 |
| 9 3 1 | 7 8 5 | 2 6 4 |
| 5 8 7 | 2 4 6 | 1 9 3 |
-------------------------
| 4 7 6 | 9 2 3 | 5 8 1 |
| 8 1 5 | 6 7 4 | 9 3 2 |
| 3 2 9 | 5 1 8 | 6 4 7 |
-------------------------
| 2 6 3 | 8 9 1 | 4 7 5 |
| 7 5 4 | 3 6 2 | 8 1 9 |
| 1 9 8 | 4 5 7 | 3 2 6 |
-------------------------
@
```

Pretty.

---

Here's the full implementation as a single function, for convenience:

```bqn
Solve0 ← {
  nbs ← ⥊¨(2⥊<⌊3÷˜↕9)⊔↕9‿9                                        # 3×3 neighbours grid
  Cands ← { i‿j𝕊b: (↕10) (¬∘∊/⊣) (j⊏⍉b)∾(i⊏b)∾(b⊑˜nbs⊑˜⌊3÷˜i‿j) } # Candidates
  Empty ← {(0=𝕩)/○⥊(↕≢𝕩)}
  Prop  ← { c←1 ⋄ {c↩0 ⋄ 𝕩{𝕩≡0?0; ix𝕊b: 2⊸⌊∘≠◶⟨0, {c↩1⋄(⊑𝕩)⌾(ix⊸⊑)b}, b⟩ ix Cands b}´Empty 𝕩}•_while_{𝕊·:c} 𝕩 }
  { Rec 𝕩:
    { 0≡𝕩? 0;  ⊑¬0∊⥊𝕩? 𝕩;
      𝕊b: pt←(⊑∘⍋ ≠∘Cands⟜b¨)⊸⊑ Empty b ⋄ 0{0≢𝕩? 𝕩; Rec 𝕨⌾(pt⊸⊑)b}´pt Cands b
    } Prop𝕩
  }𝕩
}
```

It's really fast, actually. We can add a bit of benchmarking code to the bottom of the file, which just solves the board 50 times and then prints some stats:[^6]

```bqn
board ← [ 6‿0‿2‿1‿3‿0‿0‿0‿0
        , 0‿0‿0‿0‿0‿0‿2‿0‿4
        , 0‿0‿0‿0‿0‿0‿0‿9‿0
        , 0‿7‿0‿0‿2‿0‿0‿8‿0
        , 0‿0‿0‿6‿7‿0‿0‿3‿0
        , 3‿0‿9‿0‿0‿0‿6‿0‿0
        , 0‿0‿3‿0‿0‿0‿0‿0‿0
        , 0‿0‿0‿0‿6‿0‿8‿1‿0
        , 1‿9‿0‿0‿0‿7‿0‿0‿0 ]
then←•MonoTime@ ⋄ sol←board{Solve0𝕨}⍟50@ ⋄ now←•MonoTime@
•Out "Took " ∾ (6↑•Fmt d) ∾ "s/50, avg " ∾ (6↑•Fmt 50÷˜d←now-then) ∾ "s"
PP sol
```

With these in place, I get

```console
  $ cbqn sudoku.bqn
Took 0.0534s/50, avg 0.0010s
-------------------------
| 6 4 2 | 1 3 9 | 7 5 8 |
| 9 3 1 | 7 8 5 | 2 6 4 |
| 5 8 7 | 2 4 6 | 1 9 3 |
-------------------------
| 4 7 6 | 9 2 3 | 5 8 1 |
| 8 1 5 | 6 7 4 | 9 3 2 |
| 3 2 9 | 5 1 8 | 6 4 7 |
-------------------------
| 2 6 3 | 8 9 1 | 4 7 5 |
| 7 5 4 | 3 6 2 | 8 1 9 |
| 1 9 8 | 4 5 7 | 3 2 6 |
-------------------------
```

For the reference Python implementation, we can do the same thing:

```python
# …
# Bottom of the file
import time
board = [[6, 0, 2, 1, 3, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 2, 0, 4],
         [0, 0, 0, 0, 0, 0, 0, 9, 0],
         [0, 7, 0, 0, 2, 0, 0, 8, 0],
         [0, 0, 0, 6, 7, 0, 0, 3, 0],
         [3, 0, 9, 0, 0, 0, 6, 0, 0],
         [0, 0, 3, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 6, 0, 8, 1, 0],
         [1, 9, 0, 0, 0, 7, 0, 0, 0]]
then = time.time()
for _ in range(50):
    result = solve(board)
now = time.time()
print(f"{now - then:.4f}s/50, avg {(now - then) / 50:.4f}s")
print_board(result)
```

This results in:

```console
  $ python sudoku.py
0.4186s/50, avg 0.0084s
-------------------------
| 6 4 2 | 1 3 9 | 7 5 8 |
| 9 3 1 | 7 8 5 | 2 6 4 |
| 5 8 7 | 2 4 6 | 1 9 3 |
-------------------------
| 4 7 6 | 9 2 3 | 5 8 1 |
| 8 1 5 | 6 7 4 | 9 3 2 |
| 3 2 9 | 5 1 8 | 6 4 7 |
-------------------------
| 2 6 3 | 8 9 1 | 4 7 5 |
| 7 5 4 | 3 6 2 | 8 1 9 |
| 1 9 8 | 4 5 7 | 3 2 6 |
-------------------------
```


# Version 1

So, where do we go from here? Let's do something crazy and optimise the algorithm not for speed, but for beauty—and "array-ness", whatever that means—instead. I feel like that especially in this day and age, where everyone is so focused on only caring about results, no matter the cost, one needs more beautiful code in the world.

The most straightforward modification we can do is to forego the manual recursion, and instead implement the DFS as a list of boards. Appending a new board to the front corresponds to recursing down another step. The `Cands` function doesn't need change for this at all:

```bqn
nbs ← ⥊¨(2⥊<⌊3÷˜↕9)⊔↕9‿9                                        # 3×3 neighbours grid
Cands ← { i‿j𝕊b: (↕10) (¬∘∊/⊣) (j⊏⍉b)∾(i⊏b)∾(b⊑˜nbs⊑˜⌊3÷˜i‿j) } # Candidates
```

```bqn
   bs ← {𝕩⌾(0‿1⊸⊑)board}¨ 0‿1 Cands board # Index 0‿1 changes
┌─
· ┌─                    ┌─                    ┌─
  ╵ 6 4 2 1 3 0 0 0 0   ╵ 6 5 2 1 3 0 0 0 0   ╵ 6 8 2 1 3 0 0 0 0
    0 0 0 0 0 0 2 0 4     0 0 0 0 0 0 2 0 4     0 0 0 0 0 0 2 0 4
    0 0 0 0 0 0 0 9 0     0 0 0 0 0 0 0 9 0     0 0 0 0 0 0 0 9 0
    0 7 0 0 2 0 0 8 0     0 7 0 0 2 0 0 8 0     0 7 0 0 2 0 0 8 0
    0 0 0 6 7 0 0 3 0     0 0 0 6 7 0 0 3 0     0 0 0 6 7 0 0 3 0
    3 0 9 0 0 0 6 0 0     3 0 9 0 0 0 6 0 0     3 0 9 0 0 0 6 0 0
    0 0 3 0 0 0 0 0 0     0 0 3 0 0 0 0 0 0     0 0 3 0 0 0 0 0 0
    0 0 0 0 6 0 8 1 0     0 0 0 0 6 0 8 1 0     0 0 0 0 6 0 8 1 0
    1 9 0 0 0 7 0 0 0     1 9 0 0 0 7 0 0 0     1 9 0 0 0 7 0 0 0
                      ┘                     ┘                     ┘
                                                                    ┘
   6↑ ∾{𝕩⌾(0‿1⊸⊑)board}¨¨ 1‿0⊸Cands¨ bs   # recursion
┌─
· ┌─                    ┌─                    ┌─                    ┌─                    ┌─                    ┌─
  ╵ 6 5 2 1 3 0 0 0 0   ╵ 6 7 2 1 3 0 0 0 0   ╵ 6 8 2 1 3 0 0 0 0   ╵ 6 9 2 1 3 0 0 0 0   ╵ 6 7 2 1 3 0 0 0 0   ╵ 6 8 2 1 3 0 0 0 0
    0 0 0 0 0 0 2 0 4     0 0 0 0 0 0 2 0 4     0 0 0 0 0 0 2 0 4     0 0 0 0 0 0 2 0 4     0 0 0 0 0 0 2 0 4     0 0 0 0 0 0 2 0 4
    0 0 0 0 0 0 0 9 0     0 0 0 0 0 0 0 9 0     0 0 0 0 0 0 0 9 0     0 0 0 0 0 0 0 9 0     0 0 0 0 0 0 0 9 0     0 0 0 0 0 0 0 9 0
    0 7 0 0 2 0 0 8 0     0 7 0 0 2 0 0 8 0     0 7 0 0 2 0 0 8 0     0 7 0 0 2 0 0 8 0     0 7 0 0 2 0 0 8 0     0 7 0 0 2 0 0 8 0
    0 0 0 6 7 0 0 3 0     0 0 0 6 7 0 0 3 0     0 0 0 6 7 0 0 3 0     0 0 0 6 7 0 0 3 0     0 0 0 6 7 0 0 3 0     0 0 0 6 7 0 0 3 0
    3 0 9 0 0 0 6 0 0     3 0 9 0 0 0 6 0 0     3 0 9 0 0 0 6 0 0     3 0 9 0 0 0 6 0 0     3 0 9 0 0 0 6 0 0     3 0 9 0 0 0 6 0 0
    0 0 3 0 0 0 0 0 0     0 0 3 0 0 0 0 0 0     0 0 3 0 0 0 0 0 0     0 0 3 0 0 0 0 0 0     0 0 3 0 0 0 0 0 0     0 0 3 0 0 0 0 0 0
    0 0 0 0 6 0 8 1 0     0 0 0 0 6 0 8 1 0     0 0 0 0 6 0 8 1 0     0 0 0 0 6 0 8 1 0     0 0 0 0 6 0 8 1 0     0 0 0 0 6 0 8 1 0
    1 9 0 0 0 7 0 0 0     1 9 0 0 0 7 0 0 0     1 9 0 0 0 7 0 0 0     1 9 0 0 0 7 0 0 0     1 9 0 0 0 7 0 0 0     1 9 0 0 0 7 0 0 0
                      ┘                     ┘                     ┘                     ┘                     ┘                     ┘
                                                                                                                                      ┘
```

That's basically it: the rest of the function just comprises `Empty`, which we've seen above, and the sort-by idiom to initially sort the empty cells by the number of their candidates.

```bqn
Solve1 ← { 𝕊b:
  nbs ← ⥊¨(2⥊<⌊3÷˜↕9)⊔↕9‿9                                        # 3×3 neighbours grid
  Cands ← { i‿j𝕊b: (↕10) (¬∘∊/⊣) (j⊏⍉b)∾(i⊏b)∾(b⊑˜nbs⊑˜⌊3÷˜i‿j) } # Candidates
  Place ← { ix𝕊b: {𝕩⌾(ix⊸⊑)b}¨ix Cands b }
  ⊑⟨b⟩{∾𝕨⊸Place¨𝕩}´ (⍒ ≠∘Cands⟜b⊸⋈¨)⊸⊏ (0=b)/○⥊(↕≢b)
}
```

```bqn
   (Solve0 board) ≡ Solve1 board
1
```

To more easily test new solutions, we can augment the solution file with a tiny commandline interface:

```bqn
# … code …
slv ← (•ParseFloat⊑•args)⊑⟨Solve0,Solve1⟩
then←•MonoTime@ ⋄ sol←board{Slv𝕨}⍟50@ ⋄ now←•MonoTime@
•Out "Took " ∾ (6↑•Fmt d) ∾ "s/50, avg " ∾ (6↑•Fmt 50÷˜d←now-then) ∾ "s"
PP sol
```

```console
$ cbqn sudoku.bqn 1
Took 5.2216s/50, avg 0.1044s
-------------------------
| 6 4 2 | 1 3 9 | 7 5 8 |
| 9 3 1 | 7 8 5 | 2 6 4 |
| 5 8 7 | 2 4 6 | 1 9 3 |
-------------------------
| 4 7 6 | 9 2 3 | 5 8 1 |
| 8 1 5 | 6 7 4 | 9 3 2 |
| 3 2 9 | 5 1 8 | 6 4 7 |
-------------------------
| 2 6 3 | 8 9 1 | 4 7 5 |
| 7 5 4 | 3 6 2 | 8 1 9 |
| 1 9 8 | 4 5 7 | 3 2 6 |
-------------------------
```

It's slower by a factor of 100! This should not be too surprising—there is nothing like the early pruning of `Prop` in this solution, so the list of possible boards to look at becomes quite long indeed.[^7] Though, technically, this solution is a bit more general: we don't exit early, so instead of the first one we happened to stumble across, `Solve1` outputs all solutions (although of course a good Sudoku puzzle should ideally only have a single solution.)


# Version 2

The obvious next step is to eliminate the coordinates completely, and come up with an actual array-oriented solution to the problem. One way to do this is to utilise boolean masks, which are used for a lot of things around these parts, from filtering to control flow.[^8]

The algorithm I'll present here is not my doing in any way: it seems to be attributed to Arthur Whitney; see the respective [Dyalog Workspace](https://dfns.dyalog.com/n_sudoku.htm), or the famous(?) [YouTube video](https://www.youtube.com/watch?v=DmT80OseAGs) for more.

To start, let's nicely partition the Sudoku grid into its 3×3 sub-components:

```bqn
   3 / 3‿3⥊↕9
┌─
╵ 0 1 2
  0 1 2
  0 1 2
  3 4 5
  3 4 5
  3 4 5
  6 7 8
  6 7 8
  6 7 8
        ┘
   3 /˘ 3 / 3‿3⥊↕9
┌─
╵ 0 0 0 1 1 1 2 2 2
  0 0 0 1 1 1 2 2 2
  0 0 0 1 1 1 2 2 2
  3 3 3 4 4 4 5 5 5
  3 3 3 4 4 4 5 5 5
  3 3 3 4 4 4 5 5 5
  6 6 6 7 7 7 8 8 8
  6 6 6 7 7 7 8 8 8
  6 6 6 7 7 7 8 8 8
                    ┘
```

Now we can associate each cell with its row and column index, as well as its index in the 3×3 grid above:

```bqn
   ↕9‿9
┌─
╵ ⟨ 0 0 ⟩ ⟨ 0 1 ⟩ ⟨ 0 2 ⟩ ⟨ 0 3 ⟩ ⟨ 0 4 ⟩ ⟨ 0 5 ⟩ ⟨ 0 6 ⟩ ⟨ 0 7 ⟩ ⟨ 0 8 ⟩
  ⟨ 1 0 ⟩ ⟨ 1 1 ⟩ ⟨ 1 2 ⟩ ⟨ 1 3 ⟩ ⟨ 1 4 ⟩ ⟨ 1 5 ⟩ ⟨ 1 6 ⟩ ⟨ 1 7 ⟩ ⟨ 1 8 ⟩
  ⟨ 2 0 ⟩ ⟨ 2 1 ⟩ ⟨ 2 2 ⟩ ⟨ 2 3 ⟩ ⟨ 2 4 ⟩ ⟨ 2 5 ⟩ ⟨ 2 6 ⟩ ⟨ 2 7 ⟩ ⟨ 2 8 ⟩
  ⟨ 3 0 ⟩ ⟨ 3 1 ⟩ ⟨ 3 2 ⟩ ⟨ 3 3 ⟩ ⟨ 3 4 ⟩ ⟨ 3 5 ⟩ ⟨ 3 6 ⟩ ⟨ 3 7 ⟩ ⟨ 3 8 ⟩
  ⟨ 4 0 ⟩ ⟨ 4 1 ⟩ ⟨ 4 2 ⟩ ⟨ 4 3 ⟩ ⟨ 4 4 ⟩ ⟨ 4 5 ⟩ ⟨ 4 6 ⟩ ⟨ 4 7 ⟩ ⟨ 4 8 ⟩
  ⟨ 5 0 ⟩ ⟨ 5 1 ⟩ ⟨ 5 2 ⟩ ⟨ 5 3 ⟩ ⟨ 5 4 ⟩ ⟨ 5 5 ⟩ ⟨ 5 6 ⟩ ⟨ 5 7 ⟩ ⟨ 5 8 ⟩
  ⟨ 6 0 ⟩ ⟨ 6 1 ⟩ ⟨ 6 2 ⟩ ⟨ 6 3 ⟩ ⟨ 6 4 ⟩ ⟨ 6 5 ⟩ ⟨ 6 6 ⟩ ⟨ 6 7 ⟩ ⟨ 6 8 ⟩
  ⟨ 7 0 ⟩ ⟨ 7 1 ⟩ ⟨ 7 2 ⟩ ⟨ 7 3 ⟩ ⟨ 7 4 ⟩ ⟨ 7 5 ⟩ ⟨ 7 6 ⟩ ⟨ 7 7 ⟩ ⟨ 7 8 ⟩
  ⟨ 8 0 ⟩ ⟨ 8 1 ⟩ ⟨ 8 2 ⟩ ⟨ 8 3 ⟩ ⟨ 8 4 ⟩ ⟨ 8 5 ⟩ ⟨ 8 6 ⟩ ⟨ 8 7 ⟩ ⟨ 8 8 ⟩
                                                                          ┘
   rcg ← (↕9‿9) ∾¨ 3/˘3/3‿3⥊↕9 # row-column-grid
┌─
╵ ⟨ 0 0 0 ⟩ ⟨ 0 1 0 ⟩ ⟨ 0 2 0 ⟩ ⟨ 0 3 1 ⟩ ⟨ 0 4 1 ⟩ ⟨ 0 5 1 ⟩ ⟨ 0 6 2 ⟩ ⟨ 0 7 2 ⟩ ⟨ 0 8 2 ⟩
  ⟨ 1 0 0 ⟩ ⟨ 1 1 0 ⟩ ⟨ 1 2 0 ⟩ ⟨ 1 3 1 ⟩ ⟨ 1 4 1 ⟩ ⟨ 1 5 1 ⟩ ⟨ 1 6 2 ⟩ ⟨ 1 7 2 ⟩ ⟨ 1 8 2 ⟩
  ⟨ 2 0 0 ⟩ ⟨ 2 1 0 ⟩ ⟨ 2 2 0 ⟩ ⟨ 2 3 1 ⟩ ⟨ 2 4 1 ⟩ ⟨ 2 5 1 ⟩ ⟨ 2 6 2 ⟩ ⟨ 2 7 2 ⟩ ⟨ 2 8 2 ⟩
  ⟨ 3 0 3 ⟩ ⟨ 3 1 3 ⟩ ⟨ 3 2 3 ⟩ ⟨ 3 3 4 ⟩ ⟨ 3 4 4 ⟩ ⟨ 3 5 4 ⟩ ⟨ 3 6 5 ⟩ ⟨ 3 7 5 ⟩ ⟨ 3 8 5 ⟩
  ⟨ 4 0 3 ⟩ ⟨ 4 1 3 ⟩ ⟨ 4 2 3 ⟩ ⟨ 4 3 4 ⟩ ⟨ 4 4 4 ⟩ ⟨ 4 5 4 ⟩ ⟨ 4 6 5 ⟩ ⟨ 4 7 5 ⟩ ⟨ 4 8 5 ⟩
  ⟨ 5 0 3 ⟩ ⟨ 5 1 3 ⟩ ⟨ 5 2 3 ⟩ ⟨ 5 3 4 ⟩ ⟨ 5 4 4 ⟩ ⟨ 5 5 4 ⟩ ⟨ 5 6 5 ⟩ ⟨ 5 7 5 ⟩ ⟨ 5 8 5 ⟩
  ⟨ 6 0 6 ⟩ ⟨ 6 1 6 ⟩ ⟨ 6 2 6 ⟩ ⟨ 6 3 7 ⟩ ⟨ 6 4 7 ⟩ ⟨ 6 5 7 ⟩ ⟨ 6 6 8 ⟩ ⟨ 6 7 8 ⟩ ⟨ 6 8 8 ⟩
  ⟨ 7 0 6 ⟩ ⟨ 7 1 6 ⟩ ⟨ 7 2 6 ⟩ ⟨ 7 3 7 ⟩ ⟨ 7 4 7 ⟩ ⟨ 7 5 7 ⟩ ⟨ 7 6 8 ⟩ ⟨ 7 7 8 ⟩ ⟨ 7 8 8 ⟩
  ⟨ 8 0 6 ⟩ ⟨ 8 1 6 ⟩ ⟨ 8 2 6 ⟩ ⟨ 8 3 7 ⟩ ⟨ 8 4 7 ⟩ ⟨ 8 5 7 ⟩ ⟨ 8 6 8 ⟩ ⟨ 8 7 8 ⟩ ⟨ 8 8 8 ⟩
                                                                                            ┘
   5‿3⊑rcg # Fifth row, third column, and a four in the grid representation
⟨ 5 3 4 ⟩
```

This is the part where a bit of magic happens. We create an *equal* *table* of `rcg` with itself—this takes each element and piecewise compares it to all of `rcg`. This is cool, because we can now very easily create a mask for a specific cell. Above, we've already seen the stencil for the index `1‿2` in `board`:

```
┌─
╵ 6 0 2 · · · · · ·
  0 0 0 0 0 0 2 0 4
  0 0 0 · · · · · ·
  · · 0 · · · · · ·
  · · 0 · · · · · ·
  · · 9 · · · · · ·
  · · 3 · · · · · ·
  · · 0 · · · · · ·
  · · 0 · · · · · ·
                    ┘
```

We can coax a mask out of `rcg` by giving it the right row and column numbers (1 and 2), as well as the correct grid number 0:

```bqn
   0‿2‿3 =  1‿2‿0    # Do these kinds of comparisons…
⟨ 0 1 0 ⟩
   rcg   = <1‿2‿0    # …for all of rcg
┌─
╵ ⟨ 0 0 1 ⟩ ⟨ 0 0 1 ⟩ ⟨ 0 1 1 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩
  ⟨ 1 0 1 ⟩ ⟨ 1 0 1 ⟩ ⟨ 1 1 1 ⟩ ⟨ 1 0 0 ⟩ ⟨ 1 0 0 ⟩ ⟨ 1 0 0 ⟩ ⟨ 1 0 0 ⟩ ⟨ 1 0 0 ⟩ ⟨ 1 0 0 ⟩
  ⟨ 0 0 1 ⟩ ⟨ 0 0 1 ⟩ ⟨ 0 1 1 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩
  ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 1 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩
  ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 1 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩
  ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 1 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩
  ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 1 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩
  ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 1 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩
  ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 1 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩ ⟨ 0 0 0 ⟩
                                                                                            ┘
   (⊑1⊸∊)¨rcg=<1‿2‿0 # At least one of row/col/grid should match
┌─
╵ 1 1 1 0 0 0 0 0 0
  1 1 1 1 1 1 1 1 1
  1 1 1 0 0 0 0 0 0
  0 0 1 0 0 0 0 0 0
  0 0 1 0 0 0 0 0 0
  0 0 1 0 0 0 0 0 0
  0 0 1 0 0 0 0 0 0
  0 0 1 0 0 0 0 0 0
  0 0 1 0 0 0 0 0 0
                    ┘
```

As said before, the *equal* *table* just does this for every element in `rcg` simultaneously.

```bqn
   ≢rcg
⟨ 9 9 ⟩
   ≢(⊑1⊸∊)¨=⌜˜rcg
⟨ 9 9 9 9 ⟩
   4↑ <˘⊏(⊑1⊸∊)¨=⌜˜rcg # an excerpt
┌─
· ┌─                    ┌─                    ┌─                    ┌─
  ╵ 1 1 1 1 1 1 1 1 1   ╵ 1 1 1 1 1 1 1 1 1   ╵ 1 1 1 1 1 1 1 1 1   ╵ 1 1 1 1 1 1 1 1 1
    1 1 1 0 0 0 0 0 0     1 1 1 0 0 0 0 0 0     1 1 1 0 0 0 0 0 0     0 0 0 1 1 1 0 0 0
    1 1 1 0 0 0 0 0 0     1 1 1 0 0 0 0 0 0     1 1 1 0 0 0 0 0 0     0 0 0 1 1 1 0 0 0
    1 0 0 0 0 0 0 0 0     0 1 0 0 0 0 0 0 0     0 0 1 0 0 0 0 0 0     0 0 0 1 0 0 0 0 0
    1 0 0 0 0 0 0 0 0     0 1 0 0 0 0 0 0 0     0 0 1 0 0 0 0 0 0     0 0 0 1 0 0 0 0 0
    1 0 0 0 0 0 0 0 0     0 1 0 0 0 0 0 0 0     0 0 1 0 0 0 0 0 0     0 0 0 1 0 0 0 0 0
    1 0 0 0 0 0 0 0 0     0 1 0 0 0 0 0 0 0     0 0 1 0 0 0 0 0 0     0 0 0 1 0 0 0 0 0
    1 0 0 0 0 0 0 0 0     0 1 0 0 0 0 0 0 0     0 0 1 0 0 0 0 0 0     0 0 0 1 0 0 0 0 0
    1 0 0 0 0 0 0 0 0     0 1 0 0 0 0 0 0 0     0 0 1 0 0 0 0 0 0     0 0 0 1 0 0 0 0 0
                      ┘                     ┘                     ┘                     ┘
                                                                                          ┘
```

This is quite unwieldy in this 9×9×9×9 representation, but indexing into it already works: `j⊏i⊏table` returns the correct mask for index `i‿j`:

```bqn
   2⊏1⊏(⊑1⊸∊)¨=⌜˜rcg # Mask for 1‿2
┌─
╵ 1 1 1 0 0 0 0 0 0
  1 1 1 1 1 1 1 1 1
  1 1 1 0 0 0 0 0 0
  0 0 1 0 0 0 0 0 0
  0 0 1 0 0 0 0 0 0
  0 0 1 0 0 0 0 0 0
  0 0 1 0 0 0 0 0 0
  0 0 1 0 0 0 0 0 0
  0 0 1 0 0 0 0 0 0
                    ┘
   1⊏2⊏(⊑1⊸∊)¨=⌜˜rcg # Mask for 2‿1
┌─
╵ 1 1 1 0 0 0 0 0 0
  1 1 1 0 0 0 0 0 0
  1 1 1 1 1 1 1 1 1
  0 1 0 0 0 0 0 0 0
  0 1 0 0 0 0 0 0 0
  0 1 0 0 0 0 0 0 0
  0 1 0 0 0 0 0 0 0
  0 1 0 0 0 0 0 0 0
  0 1 0 0 0 0 0 0 0
                    ┘
```

We can make this behave nicer by enclosing all of those individual masks, so they are treated as scalars instead of arrays.

```bqn
   ≢ <⎉2 (⊑1⊸∊)¨=⌜˜rcg     # Enclose rank 2 subarrays
⟨ 9 9 ⟩
   1‿2 ⊑ <⎉2 (⊑1⊸∊)¨=⌜˜rcg # Normal indexing now
┌─
╵ 1 1 1 0 0 0 0 0 0
  1 1 1 1 1 1 1 1 1
  1 1 1 0 0 0 0 0 0
  0 0 1 0 0 0 0 0 0
  0 0 1 0 0 0 0 0 0
  0 0 1 0 0 0 0 0 0
  0 0 1 0 0 0 0 0 0
  0 0 1 0 0 0 0 0 0
  0 0 1 0 0 0 0 0 0
                    ┘
```

Putting everything together, our definition for the masks is

```bqn
ms ← <⎉2{⊑1∊𝕩}¨=⌜˜(↕9‿9)∾¨3/˘3/3‿3⥊↕9 # masks
```

The candidates function now needs to be adjusted a little bit, in that we can simply multiply the board by the mask to get the numbers we're interested in.

```bqn
Cands ← { (↕10) (¬∘∊/⊣) ⥊ 𝕩×𝕨⊑ms } # Index 𝕨 in board 𝕩
```

```bqn
   board ∾○< 1‿2⊑ms
┌─
· ┌─                    ┌─
  ╵ 6 0 2 1 3 0 0 0 0   ╵ 1 1 1 0 0 0 0 0 0
    0 0 0 0 0 0 2 0 4     1 1 1 1 1 1 1 1 1
    0 0 0 0 0 0 0 9 0     1 1 1 0 0 0 0 0 0
    0 7 0 0 2 0 0 8 0     0 0 1 0 0 0 0 0 0
    0 0 0 6 7 0 0 3 0     0 0 1 0 0 0 0 0 0
    3 0 9 0 0 0 6 0 0     0 0 1 0 0 0 0 0 0
    0 0 3 0 0 0 0 0 0     0 0 1 0 0 0 0 0 0
    0 0 0 0 6 0 8 1 0     0 0 1 0 0 0 0 0 0
    1 9 0 0 0 7 0 0 0     0 0 1 0 0 0 0 0 0
                      ┘                     ┘
                                              ┘
   board×1‿2⊑ms
┌─
╵ 6 0 2 0 0 0 0 0 0
  0 0 0 0 0 0 2 0 4
  0 0 0 0 0 0 0 0 0
  0 0 0 0 0 0 0 0 0
  0 0 0 0 0 0 0 0 0
  0 0 9 0 0 0 0 0 0
  0 0 3 0 0 0 0 0 0
  0 0 0 0 0 0 0 0 0
  0 0 0 0 0 0 0 0 0
                    ┘
   ⍷⥊board×1‿2⊑ms
⟨ 6 0 2 4 9 3 ⟩
   1‿2 Cands board
⟨ 1 5 7 8 ⟩
```

The `Place` function can be adopted verbatim from `Solve1` (it's even inlined in the code below, as it's only used once), and the rest is exactly what we've seen before: get all empty cells, sort them by the number of their candidates, fold over that list, and we're done.

```bqn
Solve2 ← {
  ms ← <⎉⟨2⟩{⊑1∊𝕩}¨=⌜˜(↕9‿9)∾¨3/˘3/3‿3⥊↕9 # masks
  Cands ← { i𝕊b: (↕10) (¬∘∊/⊣) ⥊b×i⊑ms }
  ⊑ ⟨𝕩⟩{i𝕊bs: ∾{𝕊b:{𝕩⌾(i⊸⊑)b}¨i Cands b}¨bs}´ (⍒ ≠∘Cands⟜𝕩⊸⋈¨)⊸⊏ (0=𝕩)/○⥊(↕≢𝕩)
}
```

```bqn
   ∧´≡⌜⟨Solve0, Solve1, Solve2⟩{𝕎𝕩}¨<board # all equal
1
```


# Conclusion

It's not the same algorithm, so a comparison is perhaps a bit difficult, but looking at the two versions side-by-side I think we certainly improved upon the aesthetics a whole lot!

```bqn
Solve0 ← {
  nbs ← ⥊¨(2⥊<⌊3÷˜↕9)⊔↕9‿9
  Cands ← { i‿j𝕊b: (↕10) (¬∘∊/⊣) (j⊏⍉b)∾(i⊏b)∾(b⊑˜nbs⊑˜⌊3÷˜i‿j) }
  Empty ← {(0=𝕩)/○⥊(↕≢𝕩)}
  Prop  ← { c←1 ⋄ {c↩0 ⋄ 𝕩{𝕩≡0?0; ix𝕊b: 2⊸⌊∘≠◶⟨0, {c↩1⋄(⊑𝕩)⌾(ix⊸⊑)b}, b⟩ ix Cands b}´Empty 𝕩}•_while_{𝕊·:c} 𝕩 }
  { Rec 𝕩:
    { 0≡𝕩? 0;  ⊑¬0∊⥊𝕩? 𝕩;
      𝕊b: pt←(⊑∘⍋ ≠∘Cands⟜b¨)⊸⊑ Empty b ⋄ 0{0≢𝕩? 𝕩; Rec 𝕨⌾(pt⊸⊑)b}´pt Cands b
    } Prop𝕩
  }𝕩
}
```

```bqn
Solve2 ← { 𝕊b:
  ms ← <⎉2{⊑1∊𝕩}¨=⌜˜(↕9‿9)∾¨3/˘3/3‿3⥊↕9
  Cands ← { ix𝕊b: (↕10) (¬∘∊/⊣) ⥊b×ix⊑ms }
  Place ← { ix𝕊b: {𝕩⌾(ix⊸⊑)b}¨ix Cands b }
  ⊑⟨b⟩{∾𝕨⊸Place¨𝕩}´ (⍒ ≠∘Cands⟜b⊸⋈¨)⊸⊏ (0=b)/○⥊(↕≢b)
}
```

Based on some rough tests, it's only about 50× slower—but a small price to pay for beauty.

---

In case you want to play around with these functions, I've created a [repository](https://codeberg.org/slotThe/sudoku) on Codeberg.

[^1]: {-} Don't be intimidated by the number of words on this one, btw; most of those are just matrices and long tuples of coordinates.

[^2]: I'm still *so* excited about this language—it's just so cute! I mean, just look at this quine and tell me you don't feel *something*

      ```bqn
         (•Out∾⟜" "∾•Fmt) "(•Out∾⟜"" ""∾•Fmt)"
      (•Out∾⟜" "∾•Fmt) "(•Out∾⟜"" ""∾•Fmt)"
      ```

[^3]: In fact, that's how this post was written! But more about that in the future, perhaps.

[^4]: In these kinds of "REPL" code blocks, input is indented by three spaces, and output is flush to the left.

[^5]: Note that the `3↑¨` appears *after* the definition of `nbs`; it's basically only used to not have the code block be unnecessarily long.

[^6]: This is using [CBQN](https://github.com/dzaima/CBQN), commit [3bf11573](https://github.com/dzaima/CBQN/commit/3bf115731416327cce0c0ef70e2898efdb9dfc16).

[^7]: Roughly 16'000 elements, from a quick check. This is against around 20 recursion steps for `Solve0`. Go figure.

[^8]: Well, maybe technically that's the same thing, only dressed in different clothing.
