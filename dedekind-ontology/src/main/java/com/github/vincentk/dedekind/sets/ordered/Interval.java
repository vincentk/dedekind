package com.github.vincentk.dedekind.sets.ordered;

import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * @see https://en.wikipedia.org/wiki/Interval_(mathematics)
 */
public interface Interval<
E extends PoSet.Pe<E>,
A extends PoSet<E, C, A>,
B extends A,
C extends Cardinality,
T extends Interval<E, A, B, C, T>
>
extends PoSet<E, C, T>
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
    E extends PoSet.Pe<E>,
    A extends PoSet<E, C, A>,
    B extends A,
    C extends Cardinality,
    T extends HalfBounded<E, A, B, C, T>
    >
    extends Interval<E, A, B, C, T> {

	interface Left<
	E extends PoSet.Pe<E>,
	A extends PoSet<E, C, A>,
	B extends A,
	C extends Cardinality,
	T extends Left<E, A, B, C, T>
	>
	extends HalfBounded<E, A, B, C, T> {

	    /**
	     * a_min < b &forall; b &isin; B. 
	     * 
	     * @return
	     */
	    E lowerBound();
	}

	interface Right<
	E extends PoSet.Pe<E>,
	A extends PoSet<E, C, A>,
	B extends A,
	C extends Cardinality,
	T extends Right<E, A, B, C, T>
	>
	extends HalfBounded<E, A, B, C, T> {

	    /**
	     * a_max > b &forall; b &isin; B. 
	     * @return
	     */
	    E upperBound();
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
    E extends PoSet.Pe<E>,
    A extends PoSet<E, C, A>,
    B extends A,
    C extends Cardinality,
    T extends Bounded<E, A, B, C, T>
    >
    extends
    HalfBounded.Left<E, A, B, C, T>,
    HalfBounded.Right<E, A, B, C, T> {
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
    E extends PoSet.Pe<E>,
    A extends PoSet<E, C, A>,
    B extends A,
    C extends Cardinality,
    T extends HalfOpen<E, A, B, C, T>
    >
    extends HalfBounded<E, A, B, C, T> {

	interface Right<
	E extends PoSet.Pe<E>,
	A extends PoSet<E, C, A>,
	B extends A,
	C extends Cardinality,
	T extends Right<E, A, B, C, T>
	>
	extends
	HalfOpen<E, A, B, C, T>,
	HalfBounded.Left<E, A, B, C, T> {

	    /**
	     * b_min <= b &forall; b &isin; B. 
	     * 
	     * @return
	     */
	    @Override
	    E lowerBound();
	}

	interface Left<
	E extends PoSet.Pe<E>,
	A extends PoSet<E, C, A>,
	B extends A,
	C extends Cardinality,
	T extends Left<E, A, B, C, T>
	>
	extends
	HalfOpen<E, A, B, C, T>,
	HalfBounded.Right<E, A, B, C, T>
	{
	    /**
	     * b_max >= b &forall; b &isin; B. 
	     * @return
	     */
	    @Override
	    E upperBound();
	}
    }

    /**
     * Intervals with two boundary values [lower, upper],
     * both of which are in the set.
     * 
     * @param <A>
     * @param <B>
     * @param <C>
     * @param <T>
     */
    interface Closed<
    E extends PoSet.Pe<E>,
    A extends PoSet<E, C, A>,
    B extends A,
    C extends Cardinality,
    T extends Closed<E, A, B, C, T>
    >
    extends
    Bounded<E, A, B, C, T>,
    HalfOpen.Left<E, A, B, C, T>,
    HalfOpen.Right<E, A, B, C, T> {
    }
}
