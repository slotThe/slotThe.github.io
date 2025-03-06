---
title: Research
---

[^1] My research interests lie in the intersection of
category theory, Hopf algebras, and representation theory.
In particular, I like anything involving *dualities*,
like [rigid] monoidal or [\*-autonomous][^3] categories.
Amongst other things, this involves studying how algebraic gadgets lift into more categorical frameworks,
and how much theory still works in that context.
When no one's looking, I also like to study categories for their own sake.

[Hopf monads]: https://ncatlab.org/nlab/show/Hopf+monad
[\*-autonomous]: https://ncatlab.org/nlab/show/star-autonomous+category
[duoidal]: https://ncatlab.org/nlab/show/duoidal+category
[graphical calculi]: https://ncatlab.org/nlab/show/string+diagram
[linearly distributive]: https://ncatlab.org/nlab/show/linearly+distributive+category
[operads]: https://ncatlab.org/nlab/show/string+diagram
[rigid]: https://ncatlab.org/nlab/show/rigid+monoidal+category

# Papers

All of my papers—published or not—are readily available on the [arXiv].[^2]

## Published

- *[Pivotality, twisted centres, and the anti-double of a Hopf monad]* <span class="floatright">[@halbig24:pivot-hopf]</span> \
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

- *[Diagrammatics for Comodule Monads]* <span class="floatright">[@halbig23:diagr-comod-monad]</span> \
  Joint work with Sebastian Halbig.

  We extend Willerton's [@Willerton2008] graphical calculus for bimonads
  to comodule monads, a monadic interpretation of module categories
  over a monoidal category. As an application, we prove a version of
  Tannaka–Krein duality for these structures.

## Preprints

- *[Duoidal R-Matrices]* <span class="floatright">[@zorman25:duoid-r-matric]</span> \

  We define an analogue of R-matrices for bialgebras in the setting of a monad that is opmonoidal over two tensor products.
  Analogous to the classical case, such structures bijectively correspond to duoidal structures on the Eilenberg–Moore category of the monad.
  Further, we investigate how a cocommutative version of this lifts the linearly distributive structure of a normal duoidal category.

- *[Simple algebras and exact module categories]* <span class="floatright">[@coulembier25:simpl]</span> \
  Joint work with Kevin Coulembier and Mateusz Stroiński.

  We verify a conjecture of Etingof and Ostrik,
  stating that an algebra object in a finite tensor category is exact if and only if it is a finite direct product of simple algebras.
  Towards that end, we introduce an analogue of the Jacobson radical of an algebra object,
  similar to the Jacobson radical of a finite-dimensional algebra.
  We give applications of our main results in the context of incompressible finite symmetric tensor categories.

- *[Reconstruction of module categories in the infinite and non-rigid settings]* <span class="floatright">[@stroinski2024:reconstr]</span> \
  Joint work with Mateusz Stroiński.

  By building on the notions of internal projective and injective objects in a module category
  introduced by Douglas, Schommer-Pries, and Snyder,
  we extend the reconstruction theory for module categories of Etingof and Ostrik.
  More explicitly, instead of algebra objects in finite tensor categories,
  we consider quasi-finite coalgebra objects in locally finite tensor categories.
  Moreover, we show that module categories over non-rigid monoidal categories can be reconstructed via lax module monads, which generalise algebra objects.
  For the category of finite-dimensional comodules over a bialgebra, we give this result a more concrete form,
  realising module categories as categories of contramodules over Hopf trimodule algebras—this specialises to our tensor-categorical results in the Hopf case.
  Using lax module functors we give a categorical proof of the variant of the fundamental theorem of Hopf modules which applies to Hopf trimodules.
  We also give a characterisation of fusion operators for a Hopf monad as coherence cells for a module functor structure,
  using which we similarly reinterpret and reprove the Hopf-monadic fundamental theorem of Hopf modules due to Bruguières, Lack, and Virelizier.

- *[Duality in Monoidal Categories]* <span class="floatright">[@halbig23:dualit-monoid-categ]</span> \
  Joint work with Sebastian Halbig.

  We compare closed and rigid monoidal categories.
  Closedness is defined by the tensor product having a right adjoint:
  the internal hom functor.
  Rigidity, on the other hand, generalises the duality of finite-dimensional vector spaces.
  In the latter, the internal hom functor is implemented by tensoring with the respective duals.
  This raises the question:
  can one decide whether a closed monoidal category is rigid,
  simply by verifying that the internal hom is tensor-representable?
  We provide a counterexample in terms of finitely-generated projective objects in an abelian k-linear category.
  A byproduct of our work is that we obtain characterisations of the Grothendieck–Verdier duality,
  also called *-autonomy,
  and rigidity of functor categories endowed with Day convolution as their tensor product.
  Applied to Mackey functors,
  this yields a proof of a sketched argument by Bouc linking rigidity of an object to it being finitely-generated projective.

# Talks

- *2-Categorical Centre Constructions* \
  2024-11-01, Seminar "Factorisation homology", Kleinwalsertal.

  Based on [@femić23:categ-yetter] and [@street04].

- *Locally Finitely Presentable Categories and Ind-Completions* \
  2024-06-13, Seminar "Factorisation homology", Bonn.

- *The Kelly–Deligne Tensor Product* \
  2023-11-25, Seminar "Factorisation homology", Dresden.

  Based on *Tensor products of finitely cocomplete and abelian categories* by
  López Franco [@lopez13:tensor].

- *Duality in Monoidal Categories* \
  2023-01-16, Seminar GMM, Dresden. \
  2023-05-23, HATC23, Marburg. \
  2023-07-26, Uppsala Algebra Seminar, Uppsala.

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
  we will provide one.
  Additionally, a weaker version of the above statement is true:
  every monoidal category with tensor representable internal-hom is Grothendieck–Verdier.

  This talk is based on joint work with Sebastian Halbig [@halbig23:dualit-monoid-categ].

- *Abstract Schur Functors* \
  2023-07-21, "Operads" seminar, Bonn; [notes][slides:abstact-schur-functors]

  Based on the paper *Schur Functors and Categorified Plethysm* by Baez, Moeller, and Trimble
  [@baez21:schur-funct-categ-pleth].

- *Abstract Mackey Functors* \
  2023-07-15, Mackey functors seminar, Dresden; see Section 6 of the [script][slides:mackey-functors:handout].

- *Operads as Functors* \
  2022-12-15, "Operads" seminar, Bonn.

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

  Based on joint work with Sebastian Halbig [@halbig24:pivot-hopf].

- *Optics in functional programming—a categorical perspective* \
   2022-01-10, Seminar GMM, Dresden; [slides][slides:profunctor].

  A talk about the categorical aspects of (profunctor) optics, as done
  by Riley [@riley18:categ-optic] and Clark et al [@clarke20:profun-optic-categ-updat],
  as well as connections to earlier mathematical work by Pastro and Street [@pastro08:doubl].

- *Visual Category Theory* \
  2021-07-26, Seminar GMM, Dresden.

  The defense of my master's thesis, concentrating on a
  higher-dimensional graphical calculus, as first introduced by
  Willerton [@Willerton2008] and extended in the thesis.  The "basic"
  [slides][slides:visual-cat] are available—the talk was given on a
  Wacom tablet and thus contained many live drawings to illustrate the
  concepts.  These, however, are lost to time.

# Posters

- *Lax module functors, reconstruction, and Hopf algebras* \
  2024-09-02, Hopf Algebras and Monoidal Categories, Ferrara; \
  [PDF](./posters/ferrara2024/poster.pdf)
  and
  [accompanying website](./ferrara2024.html);

  Based on joint work with Mateusz Stroiński, [@stroinski2024:reconstr].

- *Pivotality, Twisted Centres, and the Anti-Double of a Hopf Monad* \
  2024-06-27, CT24, Santiago de Compostela;
  [PDF](./posters/ct2024/poster.pdf)
  and
  [accompanying website](./ct2024.html);

  Based on joint work with Sebastian Halbig—[@halbig24:pivot-hopf] and [@halbig23:diagr-comod-monad]—as well as Mateusz Stroiński, [@stroinski2024:reconstr].

- *Duality in Monoidal Categories* \
  2023-07-07, CT23, Louvain-la-Neuve;
  in [portrait][ct23:portrait] and
  [landscape][ct23:landscape] format.

  Based on a paper with Sebastian Halbig of the same name [@halbig23:dualit-monoid-categ].

# Seminars

- *Mackey Functors* \
  2023-07-14–2023-07-15, Dresden; [website here][sem:mackey-functors].

  A seminar on Mackey functors and its applications.

# Theses

- *Comodules for Categories*

  Master's thesis; with the help of a higher-dimensional graphical
  calculus, the Hopf algebraic anti-Drinfeld centre is lifted into the
  language of comodule monads.  An result relating the
  (anti-)Drinfeld centre and (anti-)Yetter--Drinfeld modules is proven
  in this monadic setting.

- *From Knot Theory to Algebra*

  Bachelor's thesis with a focus on keis; objects arising naturally when
  trying to generalize the number of 3-colourings of a knot.

[Diagrammatics for Comodule Monads]: https://arxiv.org/abs/2312.13074
[Duality in Monoidal Categories]: https://arxiv.org/abs/2301.03545
[Duoidal R-Matrices]: https://arxiv.org/abs/2503.03445
[Pivotality, twisted centres, and the anti-double of a Hopf monad]: http://www.tac.mta.ca/tac/volumes/41/4/41-04abs.html
[Reconstruction of module categories in the infinite and non-rigid settings]: https://arxiv.org/abs/2409.00793
[Simple algebras and exact module categories]: https://arxiv.org/abs/2501.06629
[ct23:landscape]: /talks/ct23-landscape.pdf
[ct23:portrait]: /talks/ct23-portrait.pdf
[sem:mackey-functors]: https://tony-zorman.com/mackey-functors
[slides:abstact-schur-functors]: /talks/abstract-schur-functors.pdf
[slides:mackey-functors:handout]: /talks/mackey-functors.pdf
[slides:piv:brno]: /talks/pivotality-in-monoidal-categories/brno.pdf
[slides:piv:prague]: /talks/pivotality-in-monoidal-categories/prague.pdf
[slides:profunctor]: /talks/profunctor-optics.pdf
[slides:visual-cat]: /talks/visual-category-theory.pdf

[^1]: {-} I may be found on the [arXiv], [orcid], and [zbmath].

[^2]: {-} A general note: one can download the source code for every paper on the arXiv.
      Clicking on "Other formats" on the relevant article will guide one through that.

[^3]: Also known as Grothendieck–Verdier duality.

[orcid]: https://orcid.org/0009-0009-4940-2864
[arXiv]: https://arxiv.org/a/zorman_t_1
[zbmath]: https://zbmath.org/authors/?q=ai%3Azorman.tony
