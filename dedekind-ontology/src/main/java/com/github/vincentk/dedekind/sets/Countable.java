package com.github.vincentk.dedekind.sets;

/**
 * A countable set. Its elements can be enumerated.
 * 
 * @param <C> cardinality
 * @param <T> implementation type
 */
public interface Countable<
E extends Set.Element<? extends E>,
C extends Cardinality.Countable,
T extends Countable<E, C, T>
>
extends Set<E, T> {
}