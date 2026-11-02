---
title: Masuoka–Wigner as an Internal Injective
date: 2026-11-02
pub: 2026-11-02
tags: maths
---

[^3]
In [@masuoka94:faith-hopf], Masuoka and Wigner study right coideal subalgebras of a Hopf algebra<!--
-->—i.e., those subalgebras \(B\) of \(A\), such that \(\Delta B \subseteq B \otimes A\).
They prove that \(A\) being faithfully flat over \(B\) is equivalent to a whole host of wonderful things.
In modern language, what they are talking about is the fact that an internal cohom has a certain shape,
which is what I want to convince you of in this post.

<!--more-->

For reference, here's the original theorem:

**Theorem (Masuoka–Wigner).**
  Let \(A\) be a Hopf algebra with \(S\) bijective, \(B \subseteq A\) a right coideal subalgebra.
  Then the following are equivalent:

  1. \({}_B A\) is flat and \(B \in \mathrm{Mod}^{A}_{B}\) is simple;
  2. \({}_B A\) is faithfully flat;
  3. \({}_B A\) is a projective generator; and
  4. \(\mathrm{Mod}^{A}_B \simeq \mathrm{Mod}^{\overline{A}}\),
     for \(\overline{A} \defeq A/AB^+\) and \(B^+ \defeq B \cap \ker \varepsilon\).

---

In more categorical language, the fact that \(A\) is faithfully flat as a left \(B\)-module just means that the functor \({-} \otimes_B A\) is faithfully exact;
i.e., if a short exact sequence \(0 \to X \to Y \to Z \to 0\) is exact if and only if  \(0 \to X \otimes_B A \to Y \otimes_B A \to Z \otimes_B A \to 0\) is exact.
Generally, an additive functor \(F\colon \mathcal{C} \to \mathcal{D}\) of abelian categories reflects exact sequences if and only if it is faithful (hence the name), and this is equivalent to it being conservative (i.e., reflecting isomorphisms) if the functor is exact.
But recall one slight reformulation of Beck's monadicity theorem,
to be found for example in Theorem 4.4.4 of [@borceux94:handb2]:

**Theorem (Beck).**
  Let \(U\colon \mathcal{D} \to \mathcal{C}\) be a functor.[^6]
  Then \(U\) is monadic if and only if it has a left adjoint, is conservative, and preserves coequalisers of \(U\)-split pairs (which must exist).

In fact, for abelian categories this takes the much more digestible form that an exact and conservative right adjoint is monadic, and an exact and conservative left adjoint is comonadic.
This means that being faithfully flat is actually about an adjunction being monadic, which quite nicely fits the fourth point of Masuoka–Wigner's theorem above.

---

Briefly, in [@stroinski2024:reconstr] we looked at reconstruction without a fibre functor,
so that—as opposed to Tannakian reconstruction—one only recovers an algebra or coalgebra object[^7] up to Morita equivalence.
This allows us to classify good module categories of monoidal categories up to that notion of equivalence:

**Theorem 5.3 of [@stroinski2024:reconstr]**
  Let \(\mathcal{C}\) be an abelian monoidal category with enough injectives
  and \(\mathcal{M}\) an abelian (left) \(\mathcal{C}\)-module category with enough injectives.
  If \(\ell \in \mathcal{M}\) is a coclosed \(\mathcal{C}\)-injective \(\mathcal{C}\)-cogenerator,
  then there is an equivalence of \(\mathcal{C}\)-module categories
  \(\mathcal{M} \simeq \mathcal{C}^{\lceil \ell,{-} \triangleright \ell \rceil}\)
  between \(\mathcal{M}\) and the Eilenberg–Moore category of the comonad
  \(\lceil \ell,{-} \triangleright \ell \rceil\) on \(\mathcal{C}\).

Although it sounds scary, a coclosed \(\mathcal{C}\)-injective \(\mathcal{C}\)-cogenerator is really just a good object such that the internal cohom[^1] is a faithfully exact functor,
and the proof is really just Beck's (co)monadicity theorem, together with some details on how the module structure works and is preserved.[^2]

In fact, up to Morita equivalence, this yields a bijective correspondence between module categories of \(\mathcal{C}\) with such a cogenerator and left exact oplax module comonads on \(\mathcal{C}\).

---

To see how these two theorems interact, we essentially set \(\mathcal{C} := \mathrm{Vec}\) and \(\mathcal{M} := \mathrm{Mod}^A_B\) in the theorem above.
In this language, \(A\) being faithfully flat is equivalent to it being a coclosed \(\mathrm{Vec}\)-injective \(\mathrm{Vec}\)-cogenerator of \(\mathrm{Mod}^A_B\);
i.e., \(\mathrm{Vec}(\lceil A, M \rceil, V) \cong \mathrm{Mod}^A_B(M, V \triangleright A)\)
holds and the functor \(\lceil A, {-} \rceil\colon \mathrm{Mod}^A_B \to \mathrm{Vec}\) is faithfully exact.

First, notice that \(A\) is always coclosed:
let \(F := {-} \otimes A\colon \mathrm{Vec} \to \mathrm{Mod}^A_B\) be the action.
We claim that this has a left adjoint
\[
  \overline{({-})} \colon \mathrm{Mod}^A_B \to \mathrm{Vec}, \qquad
  M \mapsto \overline{M} := M/MB^+.
\]
We have to prove that
\[
  \mathrm{Vec}(\overline{M}, V) \cong \mathrm{Mod}^A_B(M, V \otimes A).
\]
Let \(f\colon \overline{M} \to V\) be a linear map.
Map this to \(g\colon M \to V \otimes A\) defined by
sending \(m \in M\) to \(f(\overline{m_{(0)}}) \otimes m_{(1)}\).
Conversely, send \(g\colon M \to V \otimes A\) to \(f\colon \overline{M} \to V\)
by taking \(\overline{m} \in \overline{M}\) to \((V \otimes \varepsilon)(g(m))\).

Now, it is not too difficult to see that \(A\) being faithfully flat as a left \(B\)-module is equivalent
to it being a \(\mathrm{Vec}\)-injective \(\mathrm{Vec}\)-cogenerator.
In one direction, notice that we have a Galois-type isomorphism
\[
  M \otimes_B A \cong \overline{M} \otimes A.
\]
Since \({-} \otimes A\) is exact, if \(\lceil A, {-} \rceil = \overline{({-})}\) is faithfully exact,
then this immediately implies that \({-} \otimes_B A\) also is—at least on relative Hopf modules.
To pass to all right \(B\)-modules, one can use that
\[
  (M \otimes A)/(M \otimes A)B^+ \cong M \otimes_B A,
  \qquad
  \overline{m \otimes a} \mapsto m \otimes S(a)
\]
is a natural isomorphism, and hence \(A\) is faithfully flat.

Now, assume that \(A\) is faithfully flat.
Since \(A\) is flat as a left \(B\)-module, \({-} \otimes_B A\) is exact, and so by the Galois map above, \(\overline{({-})}\) has to be as well, so \(A\) is \(\mathrm{Vec}\)-injective.
For the cogenerator part, we instead show that \(\overline{({-})}\) is faithful.
If \(\overline{M}=0\), then due to the Galois isomorphism above we must also have \(\overline{M} \otimes A = M \otimes_B A = 0\),
and since \(A\) is faithfully flat, it follows that \(M=0\); hence, \(\overline{({-})}\) is faithful.

Then the comonad to consider is
\(\lceil A, {-} \triangleright A \rceil = \overline{{-} \otimes A} \cong {-} \otimes \overline{A}\),
because tensoring is exact.
Hence, \(\mathrm{Vec}^{\lceil A, {-} \triangleright A \rceil} \simeq \mathrm{Mod}^{\overline{A}}\),
and the theorem indeed gives \(\mathrm{Mod}^{A}_B \simeq \mathrm{Mod}^{\overline{A}}\), as required.[^5]

[^1]: The action functor \({-} \triangleright \ell\) has a *left* adjoint \(\lceil \ell, {-} \rceil\), which we call the internal cohom.

[^2]: Everything is always modulo details.

[^3]: {-} After programming, maths is also dead now,
      which I guess shouldn't keep me from writing these silly posts,
      and yet it kind of does.
      We are truly living in the darkest timeline.
      Maybe I need a circle beard to match now.

[^5]: Notice how we in fact reconstruct a coalgebra object, and not just a comonad!
      This is not the case in general, but since the category of vector spaces is the ind-completion of a rigid monoidal category—namely, the category of finite-dimensional vector spaces—all is well.

[^6]: {-} Naturally, a comonadic analogue—the one we'll actually use—also exists,
      but the monadic version is perhaps more commonly known and thus less scary.

[^7]: Really, a (co)monad, but for the purposes of this post algebra objects are good enough.
