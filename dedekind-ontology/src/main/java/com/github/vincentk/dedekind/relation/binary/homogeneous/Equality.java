package com.github.vincentk.dedekind.relation.binary.homogeneous;

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
    boolean eq(E that);
}
