package com.github.vincentk.dedekind.algebra;

import com.github.vincentk.dedekind.sets.Rings;

/**
 * @see https://en.wikipedia.org/wiki/Ring_(mathematics)
 * @param <R>
 */
public interface Ring<R extends Ring<R>> extends Rings, SemiRing<R>, Group.P<R> {
}
