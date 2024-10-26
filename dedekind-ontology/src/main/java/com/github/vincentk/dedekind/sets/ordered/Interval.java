package com.github.vincentk.dedekind.sets.ordered;

import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * @see https://en.wikipedia.org/wiki/Interval_(mathematics)
 */
public interface Interval<
A extends PoSet<C, ?>,
B extends A,
C extends Cardinality,
T extends Interval<A, B, C, T>
>
extends PoSet<C, T>
{
    /**
     * Intervals with at least one boundary value.
     * 
     * @param <A>
     * @param <B>
     * @param <C>
     * @param <T>
     */
    interface HalfBounded<
    A extends PoSet<C, ?>,
    B extends A,
    C extends Cardinality,
    T extends HalfBounded<A, B, C, T>
    >
    extends Interval<A, B, C, T> {

	interface Left<
	A extends PoSet<C, ?>,
	B extends A,
	C extends Cardinality,
	T extends Left<A, B, C, T>
	>
	extends HalfBounded<A, B, C, T> {

	    /**
	     * a_min < b &forall; b &isin; B. 
	     * 
	     * @return
	     */
	    A lowerBound();
	}

	interface Right<
	A extends PoSet<C, ?>,
	B extends A,
	C extends Cardinality,
	T extends Right<A, B, C, T>
	>
	extends HalfBounded<A, B, C, T> {

	    /**
	     * a_max > b &forall; b &isin; B. 
	     * @return
	     */
	    A upperBound();
	}
    }

    /**
     * Intervals with two boundary values (lower, upper).
     * 
     * @param <A>
     * @param <B>
     * @param <C>
     * @param <T>
     */
    interface Bounded<
    A extends PoSet<C, ?>,
    B extends A,
    C extends Cardinality,
    T extends Bounded<A, B, C, T>
    >
    extends
    HalfBounded.Left<A, B, C, T>,
    HalfBounded.Right<A, B, C, T> {
    }

    /**
     * Intervals where at least one boundary value is in the set.
     * 
     * @param <A>
     * @param <B>
     * @param <C>
     * @param <T>
     */
    interface HalfOpen<
    A extends PoSet<C, ?>,
    B extends A,
    C extends Cardinality,
    T extends HalfOpen<A, B, C, T>
    >
    extends HalfBounded<A, B, C, T> {

	interface Right<
	A extends PoSet<C, ?>,
	B extends A,
	C extends Cardinality,
	T extends Right<A, B, C, T>
	>
	extends
	HalfOpen<A, B, C, T>,
	HalfBounded.Left<A, B, C, T> {

	    /**
	     * b_min <= b &forall; b &isin; B. 
	     * 
	     * @return
	     */
	    @Override
	    B lowerBound();
	}

	interface Left<
	A extends PoSet<C, ?>,
	B extends A,
	C extends Cardinality,
	T extends Left<A, B, C, T>
	>
	extends
	HalfOpen<A, B, C, T>,
	HalfBounded.Right<A, B, C, T>
	{
	    /**
	     * b_max >= b &forall; b &isin; B. 
	     * @return
	     */
	    @Override
	    B upperBound();
	}
    }

    /**
     * Intervals with two boundary values (lower, upper),
     * both of which are in the set.
     * 
     * @param <A>
     * @param <B>
     * @param <C>
     * @param <T>
     */
    interface Closed<
    A extends PoSet<C, ?>,
    B extends A,
    C extends Cardinality,
    T extends Closed<A, B, C, T>
    >
    extends
    Bounded<A, B, C, T>,
    HalfOpen.Left<A, B, C, T>,
    HalfOpen.Right<A, B, C, T> {
    }
}
