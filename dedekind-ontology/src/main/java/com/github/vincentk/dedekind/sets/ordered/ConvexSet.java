package com.github.vincentk.dedekind.sets.ordered;

import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * A {@link ConvexSet} is a generalization of an interval on the real number line
 * to partially ordered sets ({@link PoSet}.
 * 
 * The following property holds for a convex set I &sub; P:
 * <p>
 * &forall; x, y &isin; I: x &le; z &le; y => z &isin; I. 
 * </p>
 * @see https://en.wikipedia.org/wiki/Interval_(mathematics)
 * @see https://en.wikipedia.org/wiki/Partially_ordered_set#Intervals
 */
public interface ConvexSet<
E extends PoSet.Pe<E>,
A extends PoSet<E, C, A>,
C extends Cardinality,
T extends ConvexSet<E, A, C, T>
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
    C extends Cardinality,
    T extends HalfBounded<E, A, C, T>
    >
    extends ConvexSet<E, A, C, T> {

	interface Left<
	E extends PoSet.Pe<E>,
	A extends PoSet<E, C, A>,
	C extends Cardinality,
	T extends Left<E, A, C, T>
	>
	extends HalfBounded<E, A, C, T> {

	    /**
	     * a_min < b &forall; b &isin; A. 
	     * 
	     * @return
	     */
	    E lowerBound();
	}

	interface Right<
	E extends PoSet.Pe<E>,
	A extends PoSet<E, C, A>,
	C extends Cardinality,
	T extends Right<E, A, C, T>
	>
	extends HalfBounded<E, A, C, T> {

	    /**
	     * a_max > b &forall; b &isin; A.
	     * @return
	     */
	    E upperBound();
	}
    }

    /**
     * Intervals with two boundary values (lower, upper).
     * 
     * @param <A>
     * @param <C>
     * @param <T>
     */
    interface Bounded<
    E extends PoSet.Pe<E>,
    A extends PoSet<E, C, A>,
    C extends Cardinality,
    T extends Bounded<E, A, C, T>
    >
    extends
    HalfBounded.Left<E, A, C, T>,
    HalfBounded.Right<E, A, C, T> {
    }

    /**
     * Intervals where at least one boundary value is in the set.
     * 
     * @param <A>
     * @param <C>
     * @param <T>
     */
    interface HalfOpen<
    E extends PoSet.Pe<E>,
    A extends PoSet<E, C, A>,
    C extends Cardinality,
    T extends HalfOpen<E, A, C, T>
    >
    extends HalfBounded<E, A, C, T> {

	interface Right<
	E extends PoSet.Pe<E>,
	A extends PoSet<E, C, A>,
	C extends Cardinality,
	T extends Right<E, A, C, T>
	>
	extends
	HalfOpen<E, A, C, T>,
	HalfBounded.Left<E, A, C, T> {

	    /**
	     * b_min <= b &forall; b &isin; A.
	     * 
	     * @return
	     */
	    @Override
	    E lowerBound();
	}

	interface Left<
	E extends PoSet.Pe<E>,
	A extends PoSet<E, C, A>,
	C extends Cardinality,
	T extends Left<E, A, C, T>
	>
	extends
	HalfOpen<E, A, C, T>,
	HalfBounded.Right<E, A, C, T>
	{
	    /**
	     * b_max >= b &forall; b &isin; A. 
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
    C extends Cardinality,
    T extends Closed<E, A, C, T>
    >
    extends
    Bounded<E, A, C, T>,
    HalfOpen.Left<E, A, C, T>,
    HalfOpen.Right<E, A, C, T> {
    }
}
