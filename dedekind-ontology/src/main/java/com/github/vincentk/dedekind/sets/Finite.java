package com.github.vincentk.dedekind.sets;

/**
 * Finite sets have finite cardinality.
 * They are always countable (we grant the axiom of choice).
 * 
 * @param <T> implementation type
 */
public interface Finite<
E extends Element<E>,
C extends Cardinality.Finite,
T extends Finite<E, C, T>
>
extends
Countable<E, C, T>,
Cardinality.Finite
{
}