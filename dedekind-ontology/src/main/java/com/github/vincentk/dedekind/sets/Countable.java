package com.github.vincentk.dedekind.sets;

/**
 * A countable set. Its elements can be enumerated.
 * 
 * @param <C> cardinality
 * @param <T> implementation type
 */
public interface Countable<
C extends Cardinality.Countable,
T extends Countable<C, T>
>
extends Set<T> {
}