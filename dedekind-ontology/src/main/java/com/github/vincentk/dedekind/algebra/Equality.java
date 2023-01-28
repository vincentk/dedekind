package com.github.vincentk.dedekind.algebra;

/**
 * Type-safe equality operation.
 * 
 * @param <E> type of implementation.
 */
public interface Equality<E extends Equality<E>> {

    boolean equals(E that);
}
