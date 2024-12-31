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
@SuppressWarnings("hiding")
public interface N<
//Element type:
E extends N.Natural<E>,
C extends Cardinality.Countable,
//Implementation type:
T extends N<E, C, T>
>
extends
SemiRings.Naturals,
Countable<N.Nat, Cardinality.Countable, T>,
NumberLine<N.Nat, Cardinality.Countable, T>,
ConvexSet.HalfOpen.Right<N.Nat, T, Cardinality.Countable, T>
{
    /**
     * Elements &isin; {@link N}.
     * 
     * @param <E>
     */
    interface Natural<E extends Natural<E>>
    extends
    // addition, multiplication:
    SemiRing.SmrE<E>,
    NumberLine.Number<E>,
    // distances etc. are defined:
    MetricSpace.MeG<E, E>
    {	
    }

    @Override
    default long cardinality() {
	return Long.MAX_VALUE;
    }

    @Override
    default Nat lowerBound() {
	return ZERO;
    }

    interface Nat
    extends
    SemiRing.SmrE<Nat>,
    TotallyOrdered.Oe<Nat>,
    MetricSpace.Me<Nat, Nat>
    {
	public long integer();

	@Override
	default int compareTo(Nat o) {
	    return Long.compare(integer(), o.integer());
	}

	@Override
	default Ne plus(Nat that) {
	    return nat(integer() + that.integer());
	}

	@Override
	default Ne times(Nat that) {
	    return nat(integer() * that.integer());
	}

	@Override
	default boolean eq(Nat that) {
	    return integer() == that.integer();
	}

	@Override
	default Ne distance(Nat other) {
	    return nat(Math.abs(integer() - other.integer()));
	}

	default Int64 asInt() {
	    return Z64.integer(integer());
	}
    }

    record Ne (long integer) implements Nat {

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
