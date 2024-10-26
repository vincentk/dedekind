package com.github.vincentk.dedekind.algebra.structures;


import com.github.vincentk.dedekind.algebra.sets.Rings;

/**
 * @see https://en.wikipedia.org/wiki/Ring_(mathematics)
 * @param <R>
 */
public interface Ring<R extends Ring<R>> extends Rings, SemiRing<R>, Group.P<R> {
    
    interface Integer<Z extends Integer<Z>> extends Ring<Z>, Integers {}
}
