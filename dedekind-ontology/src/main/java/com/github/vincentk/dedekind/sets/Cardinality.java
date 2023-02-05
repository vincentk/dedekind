package com.github.vincentk.dedekind.sets;

/**
 * As in: cardinality of a set.
 * 
 * @see https://en.wikipedia.org/wiki/Cardinality
 */
public interface Cardinality {

    /**
     * @see https://en.wikipedia.org/wiki/Cardinality_of_the_continuum
     */
    interface Uncountable extends Cardinality {
    }
    
    /**
     * A countable set. Its elements can be enumerated.
     * 
     * Notably, it need not be finite.
     * 
     * Example: natural numbers.
     * 
     * @see https://en.wikipedia.org/wiki/Aleph_number
     */
    interface Countable extends Cardinality {
    }

    interface Finite extends Countable {

        long cardinality();        
    }
}
