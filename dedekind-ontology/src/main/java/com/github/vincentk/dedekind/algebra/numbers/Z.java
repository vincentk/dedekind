/**
 * 
 */
package com.github.vincentk.dedekind.algebra.numbers;

import com.github.vincentk.dedekind.algebra.structures.Ring;
import com.github.vincentk.dedekind.geometry.MetricSpace;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered;

/**
 * The integer numbers.
 */
public interface Z extends
NumberLine<Z.Int, Cardinality.Countable, Z>, Ring.Integer<Z.Int, Z> {

    interface Int
    extends
    Ring.Re<Int>,
    TotallyOrdered.Oe<Int>,
    MetricSpace.MeG<Int, Int>
    {
	public long intValue();	

	@Override
	default Int plus(Int that) {
	    return integer(intValue() + that.intValue());
	}

	@Override
	default Int times(Int that) {
	    return integer(intValue() * that.intValue());
	}

	@Override
	default Int negate() {
	    return integer(-intValue());
	}

	@Override
	default int compareTo(Int o) {
	    return Long.compare(intValue(), o.intValue());
	}

	@Override
	default boolean eq(Int that) {
	    return intValue() == that.intValue();
	}

	@Override
	default Int abs() {
	    return intValue() >= 0 ? this : this.neg();
	}
    }


    record Impl (long intValue) implements Int {

    }

    static Int integer(long n) {
	return new Impl(n);
    }

    public static final Int ZERO = integer(0), ONE = integer(1), TWO = integer(2), THREE = integer(3);
}
