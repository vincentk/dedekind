/**
 * 
 */
package com.github.vincentk.dedekind.algebra.numbers;

import com.github.vincentk.dedekind.algebra.sets.Rings;
import com.github.vincentk.dedekind.algebra.structures.Ring;
import com.github.vincentk.dedekind.geometry.MetricSpace;
import com.github.vincentk.dedekind.geometry.NumberLine;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Set;
import com.github.vincentk.dedekind.sets.ordered.ConvexSet;

/**
 * The integer numbers.
 */
@SuppressWarnings("hiding")
public interface Z<
// Element type:
E extends Z.Integer<E>,
C extends Cardinality.Countable,
// Implementation type:
T extends Z<E, C, T>
>
extends
Rings.Integers,
NumberLine<E, C, T>
{
    /**
     * Elements &isin; {@link Z}.
     * 
     * @param <E>
     */
    interface Integer<E extends Integer<E>>
    extends
    // addition, multiplication:
    Ring.Re<E>,
    NumberLine.Number<E>,
    // distances etc. are defined:
    MetricSpace.MeG<E, E>
    {	
    }

    /**
     * The subset of integers that can be
     * represented in 64-bits (i.e. as java long values).
     */
    interface Z64
    extends
    Z<Z64.Int64, Cardinality.Finite, Z64>,
    ConvexSet.Closed<Z64.Int64, Z64, Cardinality.Finite, Z64>
    {
	public interface Int64
	extends Integer<Int64>
	{
	    public long intValue();	

	    @Override
	    default Int64 plus(Int64 that) {
		return integer(intValue() + that.intValue());
	    }

	    @Override
	    default Int64 times(Int64 that) {
		return integer(intValue() * that.intValue());
	    }

	    @Override
	    default Int64 negate() {
		return integer(-intValue());
	    }

	    @Override
	    default int compareTo(Int64 o) {
		return Long.compare(intValue(), o.intValue());
	    }

	    @Override
	    default boolean eq(Int64 that) {
		return intValue() == that.intValue();
	    }

	    @Override
	    default Int64 abs() {
		return intValue() >= 0 ? this : this.neg();
	    }
	}

	record Impl (long intValue) implements Int64
	{
	}

	static Int64 integer(long n) {
	    return new Impl(n);
	}

	public static final Int64
	ZERO = integer(0),
	ONE = integer(1),
	TWO = integer(2),
	THREE = integer(3),

	MIN = integer(Long.MIN_VALUE),
	MAX = integer(Long.MAX_VALUE);

	@Override
	default boolean isEmpty() {
	    return false;
	}

	@Override
	default Set<Int64, ?> intersection(Set<Int64, ?> that) {
	    return that;
	}

	@Override
	default Z64 union(Set<Int64, ?> that) {
	    return this;
	}

	@Override
	default boolean sub(Set<Int64, ?> that) {
	    return this == that;
	}

	@Override
	default boolean sup(Set<Int64, ?> that) {
	    return true;
	}

	@Override
	default Int64 upperBound() {
	    return MAX;
	}

	@Override
	default Int64 lowerBound() {
	    return MIN;
	}
    }
}
