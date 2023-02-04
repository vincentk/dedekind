/**
 * 
 */
package com.github.vincentk.dedekind.sets;

import com.github.vincentk.dedekind.sets.Rings.Integers;

public interface SemiRings {

    /**
     * Naturals are a sub-set of the integers.
     */
    interface Naturals extends Integers {}
    
    interface Booleans extends Naturals {}
}
