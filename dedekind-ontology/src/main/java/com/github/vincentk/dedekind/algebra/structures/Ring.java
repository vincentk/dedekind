package com.github.vincentk.dedekind.algebra.structures;


import com.github.vincentk.dedekind.algebra.sets.Rings;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.binary.function.operation.Closure;

/**
 * @see https://en.wikipedia.org/wiki/Ring_(mathematics)
 * @param <R>
 */
public interface Ring<
E extends Ring.Re<E>,
C extends Cardinality,
R extends Ring<E, C, R>>
extends
Rings,
SemiRing<E, C, R>,
Group.P<E, C, R>
{
    interface Re<R extends Re<R>>
    extends
    SemiRing.SmrE<R>,
    Group.P.Pe<R>
    {
	/**
	 * Subtraction (inverse of addition).
	 * 
	 * @param that
	 * @return this - that
	 */
	@Override
	default R minus(R that) {
	    return plus(that.negate());
	}

	default R 一(R that) {
	    return minus(that);
	}
    }

    
    /**
     * The integers numbers are a well-known ring.
     * They are the closure of the rational numbers under addition and multiplication.
     * @param <N>
     */
    interface Integer<
    E extends Re<E>,
    C extends Cardinality.Countable,
    Z extends Integer<E, C, Z>
    >
    extends Ring<E, C, Z>, Integers,
    Closure<C, E, Z, E, Z, Z>
    {}
}
