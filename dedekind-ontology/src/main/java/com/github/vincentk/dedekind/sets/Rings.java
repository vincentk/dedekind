package com.github.vincentk.dedekind.sets;

import com.github.vincentk.dedekind.sets.Fields.Rationals;

/**
 * Marker type denoting a <a href="https://en.wikipedia.org/wiki/Ring_(mathematics)">ring</a>.
 */
public interface Rings extends SemiRings {

    /**
     * The integers under multiplication form a
     * <a href="https://en.wikipedia.org/wiki/Commutative_ring">Commutative Ring</a>.
     */
    interface Integers extends Rings, Rationals {}
}
