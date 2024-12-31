/**
 * 
 */
package com.github.vincentk.dedekind.algebra.numbers;

import com.github.vincentk.dedekind.algebra.numbers.Z.Z64;
import com.github.vincentk.dedekind.algebra.numbers.Z.Z64.Int64;
import com.github.vincentk.dedekind.algebra.sets.SemiRings;
import com.github.vincentk.dedekind.algebra.structures.SemiRing;
import com.github.vincentk.dedekind.geometry.MetricSpace;
import com.github.vincentk.dedekind.geometry.NumberLine;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Countable;
import com.github.vincentk.dedekind.sets.ordered.ConvexSet;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered;

/**
 * The natural numbers.
 */
public interface N<
//Element type:
E extends N.Nat<E>,
S extends Cardinality.Countable,
//Implementation type:
T extends N<E, S, T>
>
extends
SemiRings.Naturals,
Countable<N.Nat63, Cardinality.Countable, T>,
NumberLine<N.Nat63, Cardinality.Countable, T>,
ConvexSet.HalfOpen.Right<N.Nat63, T, Cardinality.Countable, T>
{
    /**
     * Elements &isin; {@link N}.
     * 
     * @param <E>
     */
    interface Nat<E extends Nat<E>>
    extends
    // addition, multiplication:
    SemiRing.SmrE<E>,
    NumberLine.Number<E>,
    // distances etc. are defined:
    MetricSpace.MeG<E, E>
    {
    }

    @Override
    default Nat63 lowerBound() {
	return ZERO;
    }

    interface Nat63
    extends
    SemiRing.SmrE<Nat63>,
    TotallyOrdered.Oe<Nat63>,
    MetricSpace.Me<Nat63, Nat63>
    {
	public long integer();

	@Override
	default int compareTo(Nat63 o) {
	    return Long.compare(integer(), o.integer());
	}

	@Override
	default Ne plus(Nat63 that) {
	    return nat(integer() + that.integer());
	}

	@Override
	default Ne times(Nat63 that) {
	    return nat(integer() * that.integer());
	}

	@Override
	default boolean eq(Nat63 that) {
	    return integer() == that.integer();
	}

	@Override
	default Ne distance(Nat63 other) {
	    return nat(Math.abs(integer() - other.integer()));
	}

	default Int64 asInt() {
	    return Z64.integer(integer());
	}
    }

    record Ne (long integer) implements Nat63 {

	public Ne(long integer) {
	    assert integer >= 0;
	    this.integer = integer;
	}

    }

    static Ne nat(long n) {

	assert n >= 0;

	if (n <= 2) {
	    final int ni = (int) n;
	    switch(ni) {
	    case 0: return ZERO;
	    case 1: return ONE;
	    case 2: return TWO;
	    }
	}

	return new Ne(n);
    }

    public static final Ne ZERO = new Ne(0), ONE = new Ne(1), TWO = new Ne(2);
}
