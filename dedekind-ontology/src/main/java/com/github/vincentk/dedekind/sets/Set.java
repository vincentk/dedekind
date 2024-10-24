package com.github.vincentk.dedekind.sets;

import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.Equality;

/**
 * 
 * A set.
 * 
 * Membership is defined by delegating to the instanceof operation
 * of implementing classes.
 * 
 * @param <T> implementation type.
 * 
 * @see https://en.wikipedia.org/wiki/Set_(mathematics)
 */
public interface Set<T extends Set<T>>
extends Equality<T>
{
    @SuppressWarnings("unchecked")
    @Override
    default boolean eq(T that) {
	return ((T) this).equals(that);
    }

    /**
     * A countable set. Its elements can be enumerated.
     * 
     * @param <C> cardinality
     * @param <T> implementation type
     */
    interface Countable<
    C extends Cardinality.Countable,
    T extends Countable<C, T>
    >
    extends Set<T> {
    }

    /**
     * Finite sets have finite cardinality.
     * They are always countable (we grant the axiom of choice).
     * 
     * @param <T> implementation type
     */
    interface Finite<T extends Finite<T>>
    extends Countable<Cardinality.Finite, T>, Cardinality.Finite {
    }
}
