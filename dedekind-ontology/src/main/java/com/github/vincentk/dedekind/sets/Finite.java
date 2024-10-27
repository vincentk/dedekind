package com.github.vincentk.dedekind.sets;

/**
 * Finite sets have finite cardinality.
 * They are always countable (we grant the axiom of choice).
 * 
 * @param <T> implementation type
 */
public interface Finite<T extends Finite<T>>
extends Countable<Cardinality.Finite, T>, Cardinality.Finite {
}