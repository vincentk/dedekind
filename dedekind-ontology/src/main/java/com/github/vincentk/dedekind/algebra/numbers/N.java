/**
 * 
 */
package com.github.vincentk.dedekind.algebra.numbers;

import com.github.vincentk.dedekind.algebra.structures.MetricSpace;
import com.github.vincentk.dedekind.algebra.structures.SemiRing;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Set;
import com.github.vincentk.dedekind.sets.ordered.Interval;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered;

/**
 * The natural numbers.
 */
public interface N
extends
TotallyOrdered<Cardinality.Finite, N>,
Set.Finite<N>,
SemiRing.Natural<N>,
MetricSpace<N, N>,
Interval.HalfOpen.Right<N, N, Cardinality.Finite, N>
{

    public long integer();

    @Override
    default long cardinality() {
	return Long.MAX_VALUE;
    }

    @Override
    default Ne plus(N that) {
	return nat(integer() + that.integer());
    }

    @Override
    default Ne times(N that) {
	return nat(integer() * that.integer());
    }

    @Override
    default boolean eq(N that) {
	return integer() == that.integer();
    }

    @Override
    default N abs() {
	return this;
    }

    @Override
    default Ne distance(N other) {
	return nat(Math.abs(integer() - other.integer()));
    }

    @Override
    default N lowerBound() {
	return ZERO;
    }

    default Z asInt() {
	return Z.integer(integer());
    }

    record Ne (long integer) implements N {

	public Ne(long integer) {
	    assert integer >= 0;
	    this.integer = integer;
	}

	@Override
	public int compareTo(N o) {
	    return Long.compare(integer, o.integer());
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
