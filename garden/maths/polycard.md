---
title: Infinite Polynomial Rings
date: 2026-08-13
---

Here's something incredibly cursed: let $V$ be an infinite-dimensional vector space, say of dimension $\alpha$.
Then $V$ is isomorphic as a vector space to $k[x_{\alpha}]$, the polynomial ring in $\alpha$-many variables!
The dimension of $k[x]$ (just a single variable) is $\aleph_0$, and so the dimension of $k[x_{\alpha}]$ is $\alpha \cdot \aleph_0$.
Since $\alpha \ge \aleph_0$ this is actually equal to $\alpha$ again, so the vector spaces have the same dimension and are thus isomorphic.

---

This can be used to prove that a vector space $V$ is finite-dimensional if and only if it's isomorphic to its dual:
for $k = \mathbb{R}$ and $V = k[x]$, a good proof exists by looking at evaluation maps
$$
  \mathrm{ev}_{\lambda} \colon V \to k, \qquad p \mapsto p(\lambda),
$$
for $\lambda \in k$.
Then one can use a Lagrange polynomial argument to show these must all be linearly independent, and hence $V^*$ must have uncountable dimension.

This argument obviously depends on the ground-field being uncountable.
For smaller fields, one instead has to "enlarge" the polynomial ring so that the evaluation doesn't happen for a single $\lambda \in k$, but it's an entire variable assignment.
Then the same Lagrange polynomial argument still works, just with more indices.
