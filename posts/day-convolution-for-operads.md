---
title: The tensor product of S-modules is a convolution product
date: 2023-06-12
tags: maths
bib: true
---

Classically, (algebraic) symmetric [operads][nlab:operad] are defined as certain graded objects,
each level coming equipped with a nice action of the symmetric group,
which are also monoids in some sense.
While it is often noted that the grading and action correspond exactly to the data of a functor,
the fact that virtually all of the structure needed to define operads can be expressed categorically is often passed by,
which I think is quite the shame!
In this post I want to explicitly calculate the Day convolution for symmetric operads in the category of
vector spaces—though the argument holds for all nice enough target categories—in order
to show that it is nothing but the usual tensor product of modules.

<!--more-->

While there is more to this
story—all of which is wonderfully explained in [@kelly05:operads]—I
think focusing on the tensor product of $\mathbb{S}$-modules in the case of vector spaces already gives one a clue
as to how this whole translation works in general.

# The Day convolution product

If we have nice enough categories $\mathcal{C}$ and $\mathcal{V}$,
then the functor category $[\mathcal{C}, \mathcal{V}]$ inherits many of the properties of the two parent categories.
One of them is being *monoidal*; if there are nice functors
$$
  \otimes_{\mathcal{C}} \colon \mathcal{C} \times \mathcal{C} \to \mathcal{C},
  \qquad \qquad
  \otimes \colon \mathcal{V} \times \mathcal{V} \to \mathcal{V},
$$
that are associative and unital in appropriate ways,
then there is also a nice monoidal structure---called the *(Day) convolution product*---on $[\mathcal{C}, \mathcal{V}]$.[^2]

Intuitively,
one can think of the Day convolution much like the tensor product of vector spaces.
Given functors $F, G, H \in [\mathcal{C}, \mathcal{V}]$,
a *bilinear map* is a natural transformation
$$
  \beta ≔
  \big\{
    \beta_{v, w} \colon
      Fv \otimes Gw \to H(v \otimes_{\mathcal{C}} w)
  \big\}_{v, w \in \mathcal{V}}.
$$
Just as in the concrete case,
maps from the convolution product $F \star G$ to $H$ now correspond to exactly these bilinear maps,
and can be seen as some sort of "linearisation".

Setting $\mathcal{V} ≔ \mathsf{Vect}_{\mathtt{k}}$---for some field $\mathtt{k}$---one
can also give a definition in more explicit terms:[^7]
$$
  (F \star G)x
  ≔ \int^{c,d \in \mathcal{C}}
      \mathtt{k}\mathcal{C}(c \otimes_{\mathcal{C}} d, x) \otimes Fc \otimes Gd.
$$
The $\mathtt{k}\mathcal{C}(c \otimes_{\mathcal{C}} d, x)$ notation is meant to indicate
the linearisation of the hom-set;
i.e., we take the free vector space with basis $\mathcal{C}(c \otimes_{\mathcal{C}} d, x)$.
The little integral sign above is called a *coend*.
These are nice universal objects, and show up all the time when working with functor categories.
Still in the case of $\mathcal{V} ≔ \mathsf{Vect}_{\mathtt{k}}$,
suppose that $P \colon \mathcal{C}^{\mathrm{op}} \otimes \mathcal{C} \to \mathcal{V}$ is a functor.
In general, one can speak of the coend $\int^{c \in \mathcal{C}} P(c, c)$ of that functor;
a more explicit description can be given as a certain coequaliser:[^4]
$$
 \bigoplus_{f \colon c \to d} P(d, c) \rightrightarrows \bigoplus_{c} P(c, c) \twoheadrightarrow \int^{c} P(c, c).
$$
For a morphism $f \colon c \to d$,
the two parallel arrows are induced by
$$
  P(f, c) \colon P(d, c) \to P(c, c) \quad \text{and} \quad P(d, f) \colon P(d, c) \to P(d, d).
$$

To get a feeling for these things,
consider the following example in the case of $\mathcal{V} ≔ \mathsf{Set}$.
We know how coequalisers look in the category of sets: they are merely certain equivalence relations.
Squinting at the induced arrows,
one wants to identify $P(f, c)(x)$ with $P(d, f)(x)$,
for $c, d \in \mathcal{C}$,
$x \in P(d, c)$,
and $f \colon c \to d$.[^3]
In the special case that $P$ is the hom-functor $\mathcal{C}({-},{-})$,
the induced maps are
$$
  {-} \circ f \colon \mathcal{C}(d, c) \to \mathcal{C}(c, c)
  \qquad \text{and} \qquad
  f \circ {-} \colon \mathcal{C}(d, c) \to \mathcal{C}(d, d).
$$
More plainly, given $x \colon d \to c$ and $f \colon c \to d$,
we have $x \circ f \sim f \circ x$.
Thus, the coend here can be seen as a kind of abelianisation of arrows.

# The tensor product of $\mathbb{S}$-modules

Consider the following category $\mathbb{S}$:
objects are natural numbers,
and morphism spaces are given by $\mathbb{S}(n, m) = S_n$ if $n = m$, and $0$ otherwise,
where $S_n$ is the symmetric group of $n$ elements.

Again staying firmly in the case that $\mathcal{V} = \mathsf{Vect}_{\mathtt{k}}$,
an *$\mathbb{S}$-module* is a family of vector spaces $F = (F0, F1, F2, \dots)$,
each of which is a left $\mathtt{k}S_n$-module.
Alternatively, it is a functor from $\mathbb{S}$ to $\mathcal{V}$---this is,
of course, where the convolution product comes into play.
The category of $\mathbb{S}$-modules is usually denoted by $\mathbb{S}\text{-}\mathrm{Mod}$.

This construction might seem somewhat artificial at first,
but—as mentioned before—[(symmetric) operads][nlab:operad] turn out to be $\mathbb{S}$-modules
that are also monoids with respect to a certain monoidal structure
(not the Day convolution, but a related one).
As such, $\mathbb{S}$-modules are quite well studied as a category.

One could put quite a few monoidal structures on $\mathbb{S}$,
but what is usually called the *tensor product of $\mathbb{S}$-modules* is defined as follows:
given $F, G \in \mathbb{S}\text{-}\mathrm{Mod}$, let
$$
  (F \otimes G)r ≔ \bigoplus_{n + m = r} \mathsf{Ind}_{S_n \times S_m}^{S_r} Fn \otimes_{\mathtt{k}} Gm,
$$
where $\mathsf{Ind}_{S_n \times S_m}^{S_r}$ denotes the [induced representation][nlab:induced-rep].[^6]
Alternatively, one could write this with [shuffles][nlab:shuffle]:
$$
  (F \otimes G)r ≔ \bigoplus_{n + m = r} \mathtt{k}\mathrm{Shuf}(n, m) \otimes_{\mathtt{k}} Fn \otimes_{\mathtt{k}} Gm,
$$

This tensor product doesn't look super different than the convolution product above,
but it remains to see that the equivalence relation generated by the coequaliser really glues things together in just the right way.
Let's try that.

# Bringing everything together

In a slightly more general setting—now considering
an arbitrary (monoidal) category $\mathcal{C}$ instead of $\mathbb{S}$,
but still fixing vector spaces over $\mathtt{k}$ for
$\mathcal{V}$—the
Day convolution $F \star G$ evaluated at $x \in \mathcal{C}$ can be expressed as

$$
  \bigoplus_{\substack{f \colon a \to a' \\ g \colon b \to b'}}
    \mathtt{k}\mathcal{C}(a' \otimes b', x) \otimes_{\mathtt{k}} Fa \otimes_{\mathtt{k}} Gb
  \rightrightarrows
  \bigoplus_{a, b \in \mathcal{C}}
    \mathtt{k}\mathcal{C}(a \otimes b, x) \otimes_{\mathtt{k}} Fa \otimes_{\mathtt{k}} Gb.
$$

Looking at the induced arrows,
and---as usual---considering only elementary tensors,
on the left side we have "tuples" of
$$
  h \colon a' \otimes b' \to x,\quad  v \in Fa,\quad  w \in Gb.
$$
They are then mapped to either
$$
  h \circ (f \otimes g) \colon a \otimes b \to x,\quad  v \in Fa,\quad  w \in Gb
$$
or
$$
  h \colon a' \otimes b' \to x,\quad (Ff) v \in Fa',\quad (Gg) w \in Gb',
$$
and these two representations are identified.

In the special example of operads,
the above coequaliser is easier to
understand—remember that $\mathbb{S}$ is a category with only endomorphisms.
Thus, due to $\mathbb{S}(a + b, x)$ vanishing, all factors in the coproduct where $a + b \neq x$ are automatically 0.
As such, the whole thing transforms into

$$
  \bigoplus_{\substack{\sigma \in S_n \\ \tau \in S_m \\ n + m = r}}\!\!\!
    \mathtt{k}\mathbb{S}(n + m, r) \otimes_{\mathtt{k}} Fn \otimes_{\mathtt{k}} Gm
  \rightrightarrows
  \!\bigoplus_{n + m = r}\!\!
    \mathtt{k}\mathbb{S}(n + m, r) \otimes_{\mathtt{k}} Fn \otimes_{\mathtt{k}} Gm.
$$

The identifications
$$
  h \circ (\sigma + \tau),\quad v         ,\quad w
  \qquad\sim\qquad
  h                      ,\quad (F\sigma) v,\quad (G\tau) w
$$
now look an awful lot like identifying some left actions with some right actions.
Indeed, due to the extra condition that $n + m = r$,
we are effectively permuting $r$
by applying the action of two partitions of size $n$ and $m$---a shuffle product!
Overall, the expression
$$
  \big(\!\!\!\!
    \bigoplus_{n + m = r} \mathtt{k}\mathbb{S}(n + m, r) \otimes_{\mathtt{k}} Fn \otimes_{\mathtt{k}} Gm
  \big)
  /
  {\sim}
$$
simplifies to
$$
  \bigoplus_{n + m = r} \mathtt{k}\mathrm{Shuf}(n,m)  \otimes_{\mathtt{k}} Fn \otimes_{\mathtt{k}} Gm,
$$
or, in different notation,
$$
  \bigoplus_{n + m = r} \mathsf{Ind}_{S_n \times S_m}^{S_k} Fn \otimes_{\mathtt{k}} Gm,
$$
which is exactly the kind of formula that we wanted to end up with. Neat.

[nlab:induced-rep]: https://ncatlab.org/nlab/show/induced+representation
[nlab:operad]: https://ncatlab.org/nlab/show/operad
[nlab:shuffle]: https://ncatlab.org/nlab/show/shuffle

[^1]: For an appropriate definition of simple.

[^2]: Provided some extra technical conditions:
      $\mathcal{C}$ should be small,
      and $\mathcal{V}$ should be cocomplete,
      with the tensor product preserving colimits in both variables.
      Alternatively, one could talk about enriched categories and functors,
      but since I will work with $\mathcal{V} = \mathsf{Vect}_{\mathtt{k}}$ for most of the post,
      these more involved concepts seem overkill.

[^3]: This may or may not be an equivalence relation,
      but we can always complete to one.

[^4]: {-}        󠀠
       󠀠

      The big direct sum
      $\bigoplus_{f \colon V \to W}$ is shorthand for $\bigoplus_{V, W} \bigoplus_{f \in \mathcal{V}(V, W)}$.
      Really, this is a coproduct in general,
      but we are in $\mathsf{Vect}_{\mathtt{k}}$,
      so writing a direct sum here is appropriate.

[^6]: {-} As an aside (literally),
      equipped with the tensor product of $\mathbb{S}$-modules,
      it's not so hard to define the *plethysm product* $\odot$,
      which is actually the one that operads are monoids for:
      $$
        (F \odot G)n ≔ \bigoplus_{m \geq 0} Fm \otimes_{S_m} G^{\star m}n.
      $$
      This can, of course, also be expressed with coends:
      $$
        (F \odot G)n ≔ \int^{m} Fm \otimes_{\mathtt{k}} G^{\star m}n.
      $$
      In both expressions,
      $G^{\star m}$ is the $m$-fold tensor product (Day convolution) of $G$ with itself:
      $$
        \int^{r_1 \dots r_m}\!\! \mathbb{S}(r_1 + \dots + r_m, n) \otimes G r_1 \otimes \dots \otimes G r_m.
      $$

[^7]: Really, this definition always works,
      one just needs to substitute the $\mathtt{k}$-linearisation of the hom-set
      with a [Copower](https://ncatlab.org/nlab/show/copower).
      I have written about these things [before](https://tony-zorman.com/posts/weighted-colimits.html#copowers),
      just in a different context.
