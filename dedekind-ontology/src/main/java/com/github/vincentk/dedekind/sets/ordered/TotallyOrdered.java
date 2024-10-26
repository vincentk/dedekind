package com.github.vincentk.dedekind.sets.ordered;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.TotalOrder;

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
extends Lattice<C, T>, TotalOrder<T> {

    @Override
    default boolean leq(T that) {
	return compareTo(that) <= 0;
    }
    
    @SuppressWarnings("unchecked")
    @Override
    default T join(T that) {
	return leq(that) ? that : (T) this;
    }
    
    @SuppressWarnings("unchecked")
    @Override
    default T meet(T that) {
	return leq(that) ? (T) this : that;
    }
}