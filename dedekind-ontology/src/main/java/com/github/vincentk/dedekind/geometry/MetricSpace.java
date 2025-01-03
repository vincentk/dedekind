package com.github.vincentk.dedekind.geometry;


import com.github.vincentk.dedekind.algebra.structures.Group;
import com.github.vincentk.dedekind.algebra.structures.Magma;
import com.github.vincentk.dedekind.algebra.structures.Monoid;
import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * @see https://en.wikipedia.org/wiki/Metric_space
 * 
 * @param <M> typically something like the real numbers.
 * @param <T> often equal to {@link M}.
 */
public interface MetricSpace<
T extends Magma.Oe<T>,
C extends Cardinality,
E extends MetricSpace.Me<T, E>,
M extends MetricSpace<T, C, E, M>
>
extends
TopologicalSpace<E, C, M>
{

    interface Me<
    T extends Magma.Oe<T>,
    E extends Me<T, E>
    >
    extends
    Monoid.P.Pe<E>,
    TopologicalSpace.Me<E>
    {
	T distance(E other);
    }
    
    /**
     * Default definition of distance for elements of G+ (sets with additive inverses).
     * 
     * @param <T>
     * @param <E>
     */
    interface MeG<
    T extends Magma.M.Me<T>,
    E extends MeG<T, E>
    >
    extends
    Me<T, E>,
    Group.P.Pe<E>
    {
	/**
	 * The distance function.
	 * 
	 * @param other
	 * @return the distance from this element to the argument.
	 */
	@Override
	default T distance(E other) {
	    return minus(other).abs();
	}

	/**
	 * Typically defined for real and complex numbers
	 * (of which the naturals, ... are a subset).
	 * 
	 * @return {@link #distance(MetricSpace)} to the origin.
	 * 
	 * @see https://en.wikipedia.org/wiki/Absolute_value#Definition_and_properties
	 */
	T abs();

	/**
	 * @return a value equivalent to |x|^2
	 */
	default T abs2() {
	    final var ai = abs();
	    return ai.times(ai);
	}
    }
}
