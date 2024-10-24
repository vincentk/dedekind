package com.github.vincentk.dedekind.sets.ordered;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.relation.binary.homogeneous.TotalOrder;

/**
 * Set with a total order.
 * 
 * @param <C> cardinality
 * @param <T> implementation type
 * 
 * @see https://en.wikipedia.org/wiki/Partially_ordered_set#Derived_notions
 */
public interface TotallyOrdered<
C extends Cardinality,
T extends TotallyOrdered<C, T>
>
extends Directed<C, T>, PoSet<C, T>, TotalOrder<T> {
    
    @Override
    default boolean leq(T that) {
        return this.compareTo(that) <= 0;
    }
}