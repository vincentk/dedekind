/**
 * 
 */
package com.github.vincentk.dedekind.algebra.numbers;

import com.github.vincentk.dedekind.algebra.numbers.N.N63.Ne;
import com.github.vincentk.dedekind.algebra.numbers.Z.Z64;
import com.github.vincentk.dedekind.algebra.numbers.Z.Z64.Int64;
import com.github.vincentk.dedekind.algebra.sets.SemiRings;
import com.github.vincentk.dedekind.algebra.structures.SemiRing;
import com.github.vincentk.dedekind.geometry.MetricSpace;
import com.github.vincentk.dedekind.geometry.NumberLine;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.CountableSet;
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
CountableSet<E, Cardinality.Countable, T>,
NumberLine<E, Cardinality.Countable, T>,
ConvexSet.HalfOpen.Right<E, T, Cardinality.Countable, T>
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

    interface N63
    <
    //Element type:
    E extends N63.Nat63<E>,
    S extends Cardinality.Countable,
    //Implementation type:
    T extends N<E, S, T>
    >
    {

	interface Nat63<I extends Nat63<I>>
	extends
	Nat<I>,
	SemiRing.SmrE<I>,
	TotallyOrdered.Oe<I>,
	MetricSpace.Me<I, I>
	{
	    long integer();

	    I nat(long val);

	    @Override
	    default int compareTo(I o) {
		return Long.compare(integer(), o.integer());
	    }

	    @Override
	    default I plus(I that) {
		return nat(integer() + that.integer());
	    }

	    @Override
	    default I times(I that) {
		return nat(integer() * that.integer());
	    }

	    @Override
	    default boolean eq(I that) {
		return integer() == that.integer();
	    }

	    @Override
	    default I distance(I other) {
		return nat(Math.abs(integer() - other.integer()));
	    }

	    default Int64 asInt() {
		return Z64.integer(integer());
	    }
	}

	record Ne (long integer) implements Nat63<Ne> {

	    public Ne(long integer) {
		assert integer >= 0;
		this.integer = integer;
	    }

	    @Override
	    public
	    Ne nat(long val) {
		return N.nat(val);
	    }

	    @Override
	    public Ne abs() {
		return this;
	    }

	    @Override
	    public Ne negate() {
		assert false : "implementation not possible";
		return null;
	    }
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
