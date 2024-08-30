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

A PDF with the contents of the seminar is available
[**here**](./talks/mackey-functors.pdf).

The seminar **took place** on the 14–15th of July 2023.

<p>
<div id="border-table" class="highlight">
+--------------+--------------+--------------+
| Time         | Friday       | Saturday     |
+==============+==============+==============+
| Morning      | Manuel       | Edoardo\     |
|              |              | Sebastian    |
+--------------+--------------+--------------+
| Afternoon    | Zbiggi\      | Ivan\        |
|              | (Exercises)  | Tony         |
+--------------+--------------+--------------+
| Evening      | Exercises    |              |
|              |              |              |
+--------------+--------------+--------------+
</div>
</p>

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

# Concrete Mackey functors

## Representation theoretic preliminaries (*Manuel*)

This talk introduces much of the representation theoretic language that is needed when one wants to study classical Mackey functors.
In particular, we will see what *restriction*, *induction*, and *conjugation* look like,
and conclude with *Mackey decomposition*, modelling the interaction between these operations.
We are interested in studying the "original" Mackey functor:
for a field $\mathtt{k}$ and a finite group $G$,
the assignment $H \leq G \to M(H)$, where
$$
  M(H) = \text{Rep} H = \mathtt{k}[H]\text{-mod} = Z[\mathtt{k}[H]],
$$

The talk will follow
[@webb00:mackey],
[@thevenaz95:mackey],
Chapter 8 of [@steinberg12:repres],
Chapter 34 of [@bump13:lie],
and [@serre98:repres].

### Outline

1. *Representations, characters, and class functions*:

2. *Restriction, induction, and conjugation in terms of class functions*:

## First examples (*Zbiggi*)

An exercise session, to get a feeling for what representation theoretic Mackey functors look like.

-   Any representation of $G$.
-   The Grothendieck group $G_0$ of the group ring.
-   The (co-)homology of $G$.
-   The Burnside ring of $G$.
-   Examples from modular representation theory (rather an outlook than a main topic).

## Mackey functors and the Mackey algebra (*Sebastian*)

This talk introduces both Dress' and Green's definition of *Mackey functor*, and shows their equivalence.

### Outline

1. **Two definitions of Mackey functors**:

   - **A closer look at the Grothendieck group**:

   - **Green's definition of Mackey functors**:
     Mackey functors abstract the induction, restriction, and conjugation operations introduced on the level of Grothendieck groups.
     This becomes most transparent in the initial definition given by Green.

    - **G-sets and Dress' definition of Mackey functors**:
      The next characterisation we want to investigate is based on the notion of $G$-sets.
      In order to contextualise and motivate it, let us state some useful facts about $G$-sets first.
      The starting point is the orbit-stabiliser theorem.

    - **Equivalence of the constructions**:
      In order to prove that the two definitions of Mackey functors stated so far are equivalent,
      we will, for a brief moment, speak of *Green* and *Dress Mackey functors*.
      For any Mackey functor $(M_{\ast}, M^{\ast}) \colon G\text{-}\mathsf{Set} \to \text{Mod-}{R}$ in the sense of Dress
      we define for all $H\leq G$ the $R$-Module $M(H) = M_{\ast}(G/H)$.
      We can now define for any $K \leq H \leq G$ and $g \in G$ the prototypical operations of a Mackey functor à la Green:
      $$
        \begin{aligned}
          I_K^H &= M_{\ast}(\pi_H^K)\colon M(K) \to M(H), \\
          R^H_{K} &= M_{\ast}(\pi_H^K) \colon M(H) \to M(K), \\
          c_g^H &= M_{\ast}(c^H_g) =M^{\ast}(c^{gHg^{-1}}_{g^{-1}}) \colon M(H) \to M(gHg^{-1}).
        \end{aligned}
      $$
      Note that the two equivalent ways to define the conjugation operation is a first application of the pullback axiom for Dress Mackey functors.

2. **The Mackey algebra**:
   A representation of a quiver[^4] $\Gamma$ is
   an assignment of an $R$-module to any vertex of $\Gamma$ and
   an $R$-linear map for every edge with sources and targets given by the modules assigned to the source and target vertex, respectively.
   The similarity between this construction and Green's definition of a Mackey functor suggest that we can think of them as quiver representations.

# Abstract Mackey functors

## Relative homological algebra (*Edoardo*)

This talk is mainly based on [@dress73:contrib-ind-rep] and [@enochs00:relat].

### Outline

1. **The Dress construction**

   - **Relative projectiveness**

2. **Relative Homological Algebra**

   - **From classical to relative homological algebra**

## Day convolution (*Ivan*)

Coends play an important role whenever one wants to talk about, for example,
monoidal structures on functor categories.
The canonical tensor product on the category of Mackey functors—Day convolution—is best phrased in this language.

While coends can be defined in any appropriately enriched category<!--
-->—a bit more on this will be said in [the next talk](#abstract-mackey-functors)—<!--
-->we will specialise this for very concrete categories,
like the category of sets, or the category of (finite-dimensional) $\mathtt{k}$-vector spaces.
For a more general account see Chapter 3 of [@kelly05:basic].

This talk also follows [@loregian2021], [@richter20:from], and [@MacLane1998].

### Outline

1. **Abstract definition**:
   The first definition of (co)ends:
   in terms of universal (co)wedges.

2. **(Co)ends as (co)limits**:
   Further definitions of (co)ends
   as (co)limits in the twisted arrow category,
   and as explicit (co)equalisers.
   In particular, this has important consequences for the existence of these structures.

3. **Day convolution**:
   The canonical tensor product on a category of functors over a (nice) closed symmetric monoidal category.
   The focus here is on lots of examples, like understanding the tensor product of modules over a ring in this language.

## Abstract Mackey functors (*Tony*)

So far, all of the definitions of Mackey functor that we have seen had—from the categorical point of view—a few drawbacks:
the "original" Mackey functor was not functorial,
and Dress' definition, while featuring two functors,
split everything up between a covariant and a contravariant one.[^3]
As it turns out, there is another equivalent definition of Mackey functors—due to Lindner—featuring just a single functor as data,
with all of the necessary conditions "tucked away" in a suitable category.
The goal of this talk is to understand this definition—and the resulting category of Mackey functors—in detail.
I will follow
[@kelly05:basic],
[@lindner76:mackey],
[@loregian2021],
[@benabou67:introd],
[@panchadcharam07:mackey],
[@day70],
and [@garner22].

### Outline

1. **Day convolution redux**:
   We explore some further properties of coends,
   like the coYoneda lemma, which help us with verifying that the Day convolution is, in fact, a monoidal proudct.

2. **Enriched Category Theory**:
   A swift introduction to basic enriched category theory.
   While we mainly[^5] need the language of $\mathtt{k}$-linear categories for this seminar,
   enriched categories crop up all the time in mathematics,
   so it is still good to at least know the basic definitions.

3. **Spans**:
   We explore the category[^6] of spans over a rigid monoidal $\mathtt{k}$-linear base category.

4. **The Lindner definition**:
   With everything in place,
   the Lindner definition is introduced,
   and the resulting category of Mackey functor is endowed with a closed monoidal structure by means of Day convolution.
   Some other properties, like the $*$-autonomous structure of finite-dimensional Mackey functors, are also mentioned.

[^1]: For the bicategory of spans.

[^3]: In particular, this made morphisms between Mackey functors quite awkward to define.

[^4]: A *quiver* is a directed graph in the most general sense,
      meaning we allow things such as loops and multiple edges between the same two vertices.

[^5]: Although there are places where we care about categories enriched in commutative monoids, for example.

[^6]: Really: the classifying category of the bicategory of spans.
