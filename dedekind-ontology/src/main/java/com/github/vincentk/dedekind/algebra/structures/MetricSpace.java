package com.github.vincentk.dedekind.algebra.structures;


import com.github.vincentk.dedekind.algebra.sets.Fields;
import com.github.vincentk.dedekind.sets.Set;

/**
 * @see https://en.wikipedia.org/wiki/Metric_space
 * 
 * @param <M> typically something like the real numbers.
 * @param <T> often equal to {@link M}.
 */
public interface MetricSpace<
M extends MetricSpace<M, T>,
T extends Fields.Reals & Monoid.M<T>
>
extends Set<M> {
    
    /**
     * The distance function.
     * 
     * @param other
     * @return the distance from this element to the argument.
     */
    T distance(M other);
    
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
