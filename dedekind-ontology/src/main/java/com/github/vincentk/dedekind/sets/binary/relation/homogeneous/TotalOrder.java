package com.github.vincentk.dedekind.sets.binary.relation.homogeneous;

import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.PreOrder.Directed;

public interface TotalOrder<T extends TotalOrder<T>>
extends
PartialOrder.Strict<T>, Comparable<T>, Directed<T> {

    @Override
    default boolean leq(T that) {
	return this.compareTo(that) <= 0;
    }

    @SuppressWarnings("unchecked")
    @Override
    default T upperBound(T that) {
	return leq(that) ? that : (T) this;
    }
}
