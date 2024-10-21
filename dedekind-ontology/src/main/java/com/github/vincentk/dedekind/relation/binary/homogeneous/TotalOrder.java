package com.github.vincentk.dedekind.relation.binary.homogeneous;

import com.github.vincentk.dedekind.relation.binary.homogeneous.PreOrder.Directed;

public interface TotalOrder<T> extends Comparable<T>, Directed<T> {

    @Override
    default boolean leq(T that) {
	return this.compareTo(that) <= 0;
    }

    @SuppressWarnings("unchecked")
    @Override
    default T upperBound(T that) {
	return this.compareTo(that) <= 0 ? (T) this : that;
    }
}
