package com.github.vincentk.dedekind.algebra.structures;


import com.github.vincentk.dedekind.algebra.sets.Rings;
import com.github.vincentk.dedekind.sets.binary.function.operation.Closure;

/**
 * @see https://en.wikipedia.org/wiki/Ring_(mathematics)
 * @param <R>
 */
public interface Ringoid<
E extends Ringoid.Re<E>,
R extends Ringoid<E, R>>
extends
Rings,
SemiRing<E, R>,
Group.P<E, R>
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
    Z extends Integer<E, Z>
    >
    extends Ringoid<E, Z>, Integers,
    Closure<E, Z, E, Z, Z>
    {}
}
