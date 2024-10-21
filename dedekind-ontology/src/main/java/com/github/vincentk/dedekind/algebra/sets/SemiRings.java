package com.github.vincentk.dedekind.algebra.sets;

import com.github.vincentk.dedekind.algebra.sets.Rings.Integers;

public interface SemiRings extends Monoids {

    /**
     * Naturals are a sub-set of the integers.
     */
    interface Naturals extends Integers {}
    
    interface Booleans extends Naturals {}
}
