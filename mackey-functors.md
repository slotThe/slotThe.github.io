---
title: Mackey Functors Seminar
bib: true
---

This is the website for a seminar on *Mackey functors* held at TU Dresden,
in collaboration with the University of Marburg.
The goal was to understand several equivalent definitions of these functors,
and—along the way—introduce representation theory of finite groups,
relative homological algebra,
and some monoidal category theory.

# Roadmap

> A Mackey functor is an algebraic structure possessing operations which behave like the induction, restriction and conjugation mappings in group representation theory.
> Operations such as these appear in quite a variety of diverse contexts — for example group cohomology, the algebraic K-theory of group rings, and algebraic number theory — and it is their widespread occurrence which motivates the study of such operations in abstract.
>
> —Peter Webb – A guide to Mackey functors

This seminar is supposed to be a modest introduction to a more ambitious scheme.
The latter might be summarised under the slogan:
"Investigate algebraic structures up to homotopy using operads."
In particular,
we are interested in formalising $(\infty, 1)$-Grothendieck–Verdier categories
and express additional structures such as ribbon twists by group actions.
In doing so,
we follow the program laid out by Boyarchenko and Drinfeld
in "A duality formalism in the spirit of Grothendieck and Verdier".
A section,
contributed by Lurie,
to the aforementioned article is especially helpful
as it states profound conjectures about the shape of these theories
in the $(\infty, 1)$-categorical setting.
(Spectral) Mackey functors enter this story as their non-homotopical analogues
from a ribbon Grothendieck–Verdier category.
Thus, spectral Mackey functors are good test candidates for formulating
and checking these tools in the $(\infty, 1)$-categorical world.

This seminar will be an introduction to these topics.
It has two main parts:

1.  Concrete and abstract Mackey functors.
2.  Coend calculus and Day convolution.

# Schedule

The seminar took place on the 14–15th of July 2023.

<p>
<div id="border-table" class="highlight">
+--------------+--------------+--------------+
| Time         | Friday       | Saturday     |
+==============+==============+==============+
| Morning      | Manuel       | Edoardo\     |
|              |              | Ivan         |
+--------------+--------------+--------------+
| Afternoon    | Zbiggi\      | Tony         |
|              | (Exercises)  |              |
+--------------+--------------+--------------+
| Evening      | Sebastian    |              |
|              |              |              |
+--------------+--------------+--------------+
</div>
</p>

## Concrete Mackey functors

### Sources

-   Peter Webb: A guide to Mackey functors [@webb00:mackey]
-   Jacques Thévenaz and Peter Webb: The structure of Mackey functors [@thevenaz95:mackey]

### Outline

#### The Mackey machine in group representation theory (*Manuel*)

Chapter 8 of Representation Theory of Finite Groups by Benjamin Steinberg [@steinberg12:repres],
Chapter 34 of Lie Groups by Daniel Bump [@bump13:lie], or
Linear representations of finite groups by Jean-Pierre Serre [@serre98:repres].

#### First definitions (*Sebastian*)

-   Green Definition of Mackey functors
-   Definition of Dress and comparison with the Green definition

#### First examples (*Zbiggi*)

-   Any representation of G [Exercise]
-   The Grothendieck group of the group ring
-   The (co-)homology of G [Exercise]
-   The Burnside ring of G [important]
-   Examples from modular representation theory (rather an outlook than a main topic)

#### The Mackey algebra (*Sebastian*)

-   Equivalence of categories between Mky and $\mu$-Mod
-   Splitting of the unit into orthogonal idempotents
-   Prove that Mky has enough projectives
-   Projectivity of the Burnside Mackey functor [Exercise]
-   Semisimplicity of Mky in characteristic zero (over a field)

#### Green functors (*Edoardo*)

-   Concrete definition and examples (cohomology, Burnside Mackey functor)

#### Relative homological algebra and the Dress construction (*Edoardo*)

- Andreas Dress: Contributions to the theory of induced representations [@dress73:contrib-ind-rep]

#### Outlook: Cohomological Mackey functors and the Hecke algebra

## Abstract Mackey functors

### Sources

-   John Bénabou: Introduction to Bicategories[^1] [@benabou67:bicats]
-   Harald Lindner: A Remark on Mackey-Functors [@lindner76:mackey]
-   Max Kelly: Basic concepts of enriched category theory [@kelly05:basic]

### Outline

#### Preliminaries: Spans and functor categories (*Tony*)

-   The bicategory of spans and functor categories

#### Enriched category theory (*Tony*)

-   Definition of an enriched category, functor, natural transformation, adjunction etc.
-   Examples of enriched categories (Ab or vect enrichment, dg categories, simplicial enrichment, Cat-enriched categories as strict 2 categories)
-   The enriched Yoneda lemma
-   Weighted limits
-   Change of enrichment [Exercise]

#### The category of Mackey functors (*Tony*)

-   Lindners definition of Mackey functors
-   Naive definition of the tensor product of Mackey functors
-   Characterisations of rigid dualisability[Exercise]
-   The dual basis lemma for Mky functors[Important]

## Day convolution

### Sources

-   Brian Day: On closed categories of functors [@day70]
-   Brian Day, Elango Panchadcharam, Ross Street: Lax Braidings and the Lax Centre [@day07:lax]
-   Fosco Loregian: Coend Calculus [@loregian2021]
-   Max Kelly: On the operads of J.P. May [@kelly05:operads]

### Outline

#### Preliminaries: (Co)end calculus (*Ivan*)

-   Abstract definition of (co)ends
-   Functoriality of coends [Exercise]
-   (Co)ends as (co)limits
-   Prove the Elmendorf reconstruction theorem

#### Day convolution

-   Copowers
    -   Important example: every cocomplete category is copowered over **Set**
-   Definition
-   Day convolution as a Kan extension
-   Associated closed structure

#### Extended example: operads

A good reference to follow is Kelly's "On the operads of J.P. May" [@kelly05:operads],
or Chapter 6 of "Coend Calculus" [@loregian2021].

-   Definition of an "abstract" operad
-   Bonus (not strictly necessary): the Kelly product $\odot$ yields an example of a category
    that is left-closed, but not right-closed

#### Profunctors and promonoidal categories

## Final goals

The speakers of the seminar are invited to write a short proceedings articles.
For example the following question might be investigated:
Let $(\mathcal{C}, d)$ be a symmetric Grothendieck–Verdier category with dualising object $d$.
That is, $\mathcal{C}$ is symmetric monoidal,
and we have an anti-equivalence $D \colon \mathcal{C} \to \mathcal{C}$
such that for any $X \in \mathcal{C}$ there exists an isomorphism
$\mathrm{Hom}(A \otimes X, d) \cong \mathrm{Hom}(A, DX)$.
Further we assume $D^2 = \mathrm{Id}$.
For any $X \in \mathcal{C}$
we have a evaluation map $\mathrm{ev}_x \colon X \otimes D^{-1} X \to d$
given by pullback of the identity on $X$ under the adjunction.
This can be used to define a trace $\mathrm{tr}(f)$ for any endomorphism $f \colon X \to X$
by applying $D(\mathrm{ev}_x)$ to the image of f under the following compositions:

\begin{align*}
  \mathrm{Hom}(X,X) &\cong \mathrm{Hom}(X \otimes D^{-1} X, d)
                     \cong \mathrm{Hom}(1, D(X \otimes D^{-1}X)) \\
                    &\to \mathrm{Hom}(1, D^2 d) \cong \mathrm{Hom}(1,1).
\end{align*}

Note that this trace differs slightly from the one for pivotal categories.

In the paper "The Burnside dimension of projective Mackey functors", Bouc [@bouc05:mackey] utilises the idea of traces for the full maximal pivotal subcategory of Mackey functors.
We might try to interpret/extend his results using traces for GV categories.

[^1]: For the bicategory of spans.
