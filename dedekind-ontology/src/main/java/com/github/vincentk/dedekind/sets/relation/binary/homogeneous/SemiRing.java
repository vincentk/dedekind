package com.github.vincentk.dedekind.sets.relation.binary.homogeneous;

import com.github.vincentk.dedekind.algebra.sets.Rings;

/**
 * A semiring: basically a ring but without the additive inverse.
 * 
 * @see https://en.wikipedia.org/wiki/Semiring
 */
public interface SemiRing<R extends SemiRing<R>>
extends
Rings,
Monoid.P<R>,
Monoid.M<R>
{
    interface Natural<N extends Natural<N>> extends SemiRing<N>, Naturals {}
}
