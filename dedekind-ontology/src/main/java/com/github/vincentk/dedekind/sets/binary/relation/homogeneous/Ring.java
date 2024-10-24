package com.github.vincentk.dedekind.sets.binary.relation.homogeneous;

import com.github.vincentk.dedekind.algebra.sets.Rings;

/**
 * @see https://en.wikipedia.org/wiki/Ring_(mathematics)
 * @param <R>
 */
public interface Ring<R extends Ring<R>> extends Rings, SemiRing<R>, Group.P<R> {
    
    interface Integer<N extends Integer<N>> extends Ring<N>, Integers {}
}
