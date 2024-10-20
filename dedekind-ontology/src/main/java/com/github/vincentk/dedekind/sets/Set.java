package com.github.vincentk.dedekind.sets;

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
public interface Set<T extends Set<T>> {

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

    /**
     * Set with a total order.
     * 
     * @param <C> cardinality
     * @param <T> implementation type
     * 
     * @see https://en.wikipedia.org/wiki/Partially_ordered_set#Derived_notions
     */
    interface TotalOrder<
    C extends Cardinality,
    T extends TotalOrder<C, T>
    >
    extends Set<T>, Comparable<T> {
    }
}
