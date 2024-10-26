package com.github.vincentk.dedekind.algebra.structures;


import com.github.vincentk.dedekind.algebra.sets.Rings;
import com.github.vincentk.dedekind.sets.Set;
import com.github.vincentk.dedekind.sets.unary.function.operation.Closure;

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
    /**
     * The natural numbers are a well-known semi-ring.
     * They are the closure of the integers under addition and multiplication.
     * @param <N>
     */
    interface Natural<N extends Natural<N>>
    extends
    Set<N>,
    SemiRing<N>, Naturals,
    Closure<N, N, N>
    {}
}
