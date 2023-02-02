package com.github.vincentk.dedekind.algebra;

import com.github.vincentk.dedekind.sets.Rings;

/**
 * @see https://en.wikipedia.org/wiki/Ring_(mathematics)
 * @param <R>
 */
public interface Ring<R extends Ring<R>> extends Rings, SemiRing<R>, Group.P<R> {

    /**
     * The integers under multiplication form a
     * <a href="https://en.wikipedia.org/wiki/Commutative_ring">Commutative Ring</a>.
     */
    interface Integer<I extends Integer<I>> extends Ring<I>, Integers {}
}
