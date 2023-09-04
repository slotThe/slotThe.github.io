---
title: Research
---
[^1] My research interests can broadly be characterised under the term
"monoidal category theory"—in particular,
I like anything involving *dualities*,
like [rigid] monoidal or [\*-autonomous] categories.
Further, I enjoy studying how algebraic gadgets lift into more categorical frameworks,
and how much theory still works in that context.
When no one's looking, I also like to study categories for their own sake.
Some more keywords include [Hopf monads], [graphical calculi], [duoidal] and [linearly distributive] categories, as well as [operads].

[Hopf monads]: https://ncatlab.org/nlab/show/Hopf+monad
[\*-autonomous]: https://ncatlab.org/nlab/show/star-autonomous+category
[duoidal]: https://ncatlab.org/nlab/show/duoidal+category
[graphical calculi]: https://ncatlab.org/nlab/show/string+diagram
[linearly distributive]: https://ncatlab.org/nlab/show/linearly+distributive+category
[operads]: https://ncatlab.org/nlab/show/string+diagram
[rigid]: https://ncatlab.org/nlab/show/rigid+monoidal+category
[zbmath]: https://zbmath.org/authors/?q=ai%3Azorman.tony

# Preprints

All of my preprints are readily available on the [arXiv].[^2]

- *[Duality in Monoidal Categories]* \
   Joint work with Sebastian Halbig.

   We compare closed and rigid monoidal categories.  Closedness is
   defined by the tensor product having a right adjoint: the
   internal-hom functor.  Rigidity on the other hand generalises the
   concept of duals in the sense of finite-dimensional vector spaces.  A
   consequence of these axioms is that the internal-hom functor is
   implemented by tensoring with the respective duals.  This raises the
   question: can one decide whether a closed monoidal category is rigid,
   simply by verifying that the internal-hom is tensor-representable?
   At the *Research School on Bicategories, Categorification and Quantum
   Theory*, Heunen suggested that this is not the case.  In this note,
   we will prove his claim by constructing an explicit counterexample.

- *[Pivotality, twisted centres and the anti-double of a Hopf monad]* \
  Joint work with Sebastian Halbig.

  Finite-dimensional Hopf algebras admit a correspondence between
  so-called pairs in involution, one-dimensional anti-Yetter–Drinfeld
  modules and algebra isomorphisms between the Drinfeld and
  anti-Drinfeld double.  We extend it to general rigid monoidal
  categories and provide a monadic interpretation under the assumption
  that certain coends exist.  Hereto we construct and study the
  anti-Drinfeld double of a Hopf monad.  As an application the
  connection with the pivotality of Drinfeld centres and their
  underlying categories is discussed.

[arXiv]: https://arxiv.org/a/zorman_t_1

# Talks

- *Abstract Schur Functors* \
  2023-07-21, Block seminar "Operads", Bonn.

  Based on [this paper](https://arxiv.org/abs/2106.00190) by Baez, Moeller, and Trimble.

- *Abstract Mackey Functors* \
  2023-07-15, Mackey functors seminar, Dresden; see Section 6 of the [script][slides:mackey-functors:handout].

- *Duality in Monoidal Categories* \
  2023-01-16, Seminar GMM, Dresden. \
  2023-05-23, HATC23, Marburg;
  [slides][slides:duality] ([handout][slides:duality:handout]). \

  Dualities are an important tool in the study of monoidal categories and their applications.
  For example, underlying the construction of Tor and Ext functors
  is the tensor–hom adjunction in the category of bimodules over a unital
  ring—this is referred to as a closed monoidal structure.
  A stronger concept, rigidity, models the behaviour of finite-dimensional vector spaces;
  that is, the existence of evaluation and coevaluation morphisms,
  implementing a notion of dual basis.
  Under delooping, this corresponds to the concept of an adjunction in a bicategory,
  with coevaluation as unit and evaluation as counit.
  Grothendieck–Verdier duality,
  also called *-autonomy,
  lies between the strict confinements of rigidity,
  and the generality of monoidal closedness.
  It is closely linked to linearly distributive categories with negation.

  An immediate consequence of rigidity is that the internal-hom functor is tensor representable.
  That is, a dualising functor sending any object to its dual exists,
  and tensoring with the object is left adjoint to tensoring with its dual.

  This raises a naive question:

    > Is a monoidal category with tensor representable internal-hom automatically rigid?

  While it is expected that this is not true in general,
  constructing counterexamples is non-trivial;
  we will provide one in the form of the category of Mackey functors.
  Additionally, a weaker version of the above statement is true:
  every monoidal category with tensor representable internal-hom is Grothendieck–Verdier.

  This talk is based on
  [joint work with Sebastian Halbig](https://arxiv.org/abs/2301.03545).

- *Operads as Functors* \
  2022-12-15, Block seminar "Operads", Bonn.

- *Pivotality, twisted centres and the anti-double of a Hopf monad* \
  2022-05-12, Seminar of the Czech Academy of Sciences, Prague; [slides][slides:piv:prague]. \
  2022-05-15, PSSL 106, Brno; [slides][slides:piv:brno]. \
  2022-05-30, QGS: Quantum Group Seminar, Online. \

  Pairs in involution are an algebraic structure whose systematic study
  is motivated by their applications in knot theory, representation
  theory and cyclic homology theories.  We will explore a categorical
  version of these objects from the perspective of representation theory
  of monoidal categories.  A focus will lie on illustrating how their
  existence is linked to a particular well-behaved notion of duality,
  called pivotality.  As a central point, we show how the language of
  monads allows us to combine the algebraic and categorical perspective
  on such pairs.

  Based on [joint work with Sebastian Halbig](https://arxiv.org/abs/2201.05361).

- *Optics in functional programming—a categorical perspective* \
   2022-01-10, Seminar GMM, Dresden; [slides][slides:profunctor].

  A talk about the categorical aspects of (profunctor) optics, as done
  by [Riley] and [Clark et.al.], as well as connections to earlier
  mathematical work by [Pastro and Street].

- *Visual Category Theory* \
  2021-07-26, Seminar GMM, Dresden.

  The defense of my master's thesis, concentrating on a
  higher-dimensional graphical calculus, as first introduced by
  [Willerton] and extended in the thesis.  The "basic"
  [slides][slides:visual-cat] are available—the talk was given on a
  Wacom tablet and thus contained many live drawings to illustrate the
  concepts.  These, however, are lost to time.

# Posters

- *Duality in Monoidal Categories* \
  2023-07-07, CT23, Louvain-la-Neuve;
  in [portrait][ct23:portrait] and
  [landscape][ct23:landscape] format.

  Based on a paper with Sebastian Halbig of the [same name][Duality in Monoidal Categories].

# Seminars

- *Mackey Functors* \
  2023-07-14–2023-07-15, Dresden; [website here][sem:mackey-functors].

  A seminar on Mackey functors and its applications.

# Theses

- *Comodules for Categories*

  Master's thesis; with the help of a higher-dimensional graphical
  calculus, the Hopf algebraic anti-Drinfeld centre is lifted into the
  language of comodule monads.  An interesting equivalence relating the
  (anti-)Drinfeld centre and (anti-)Yetter--Drinfeld modules is proven
  in this monadically enriched setting.

- *From Knot Theory to Algebra*

  Bachelor's thesis with a focus on keis; objects arising naturally when
  trying to generalize the number of 3-colourings of a knot.

[Clark et.al.]: https://arxiv.org/abs/2001.07488
[Duality in Monoidal Categories]: https://arxiv.org/abs/2301.03545
[Pastro and Street]: https://arxiv.org/abs/0711.1859
[Pivotality, twisted centres and the anti-double of a Hopf monad]: https://arxiv.org/abs/2201.05361
[Riley]: https://arxiv.org/abs/1809.00738
[Willerton]: https://arxiv.org/abs/0807.0658
[ct23:landscape]: /talks/ct23-landscape.pdf
[ct23:portrait]: /talks/ct23-portrait.pdf
[sem:mackey-functors]: https://tony-zorman.com/mackey-functors
[slides:duality:handout]: /talks/duality-in-monoidal-categories-handout.pdf
[slides:duality]: /talks/duality-in-monoidal-categories.pdf
[slides:mackey-functors:handout]: /talks/mackey-functors.pdf
[slides:piv:brno]: /talks/pivotality-in-monoidal-categories/brno.pdf
[slides:piv:prague]: /talks/pivotality-in-monoidal-categories/prague.pdf
[slides:profunctor]: /talks/profunctor-optics.pdf
[slides:visual-cat]: /talks/visual-category-theory.pdf

[^1]: {-} I may be found on the [arXiv] and [zbmath].

[^2]: {-} A general note: one can download the source code for every
          paper on the arXiv.  Clicking on "Other formats" on the
          relevant article will guide one through that.
