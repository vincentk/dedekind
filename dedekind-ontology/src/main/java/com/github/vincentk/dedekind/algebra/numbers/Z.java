/**
 * 
 */
package com.github.vincentk.dedekind.algebra.numbers;

import com.github.vincentk.dedekind.algebra.structures.Ring;
import com.github.vincentk.dedekind.geometry.MetricSpace;
import com.github.vincentk.dedekind.geometry.NumberLine;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered;

/**
 * The integer numbers.
 */
public interface Z<
E extends Z.Integer<E>
>
extends
NumberLine<E, Cardinality.Countable, Z<E>>,
Ring.Integer<E, Z<E>>
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
    // total order:
    TotallyOrdered.Oe<E>,
    // distance is defined:
    MetricSpace.MeG<E, E>
    {	
    }

    interface Z64 extends Z<Z64.Int64> {
	//}

	interface Int64
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

	record Impl (long intValue) implements Int64 {

	}

	static Int64 integer(long n) {
	    return new Impl(n);
	}

	public static final Int64
	ZERO = integer(0),
	ONE = integer(1),
	TWO = integer(2),
	THREE = integer(3);
    }


}
