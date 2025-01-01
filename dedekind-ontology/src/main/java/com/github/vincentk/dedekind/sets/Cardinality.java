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

	interface PowerOfTwo
	extends Finite
	{
	    /**
	     * 64 bits <=> 2^64
	     */
	    interface B64 extends PowerOfTwo { 
	    }
	    
	    /**
	     * 64 bits <=> 2^63 i.e. enumerable by the (signed) 64-bit long.
	     */
	    interface B63 extends B64 { 

		long cardinality(); 
	    }
	}
    }

    interface Small extends Finite.PowerOfTwo.B64 {

	interface Two extends B63 {

	    @Override
	    default long cardinality() {
		return 2;
	    }
	}
	
	interface One extends Two {

	    @Override
	    default long cardinality() {
		return 1;
	    }
	}

	interface Empty extends One {

	    @Override
	    default long cardinality() {
		return 0;
	    }
	}

    }
}
