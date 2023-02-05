/**
 * 
 */
package com.github.vincentk.dedekind.numbers;

import com.github.vincentk.dedekind.sets.Fields;
import com.github.vincentk.dedekind.sets.Set;

/**
 * @see https://en.wikipedia.org/wiki/Metric_space
 * 
 * @param <M> typically something like the real numbers.
 * @param <T>
 */
public interface MetricSpace<M extends MetricSpace<M, T>, T extends Fields.Reals> extends Set<M> {
    
    /**
     * The distance function.
     * 
     * @param other
     * @return the distance from this element to the argument.
     */
    T distance(M other);
}
