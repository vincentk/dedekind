package com.github.vincentk.dedekind.sets.binary.relation.homogeneous;

/**
 * Type-safe equality operation.
 * 
 * @param <E> type of implementation.
 */
public interface Equality<E extends Equality<E>> {

    /**
     * @param that
     * @return true exactly if this and that are equal.
     */
    default boolean eq(E that) {
	return equals(that);
    }
}
