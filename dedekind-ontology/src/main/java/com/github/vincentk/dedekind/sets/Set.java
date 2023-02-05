package com.github.vincentk.dedekind.sets;

import java.util.Optional;

/**
 * 
 * A set. Membership is defined by delegating to the instanceof operation.
 * 
 * @param <T> implementation type.
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
    extends Set<T>{

        Optional<T> next();
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
     * Partially ordered set.
     * 
     * @param <C> cardinality
     * @param <T> implementation type
     */
    interface Po<
    C extends Cardinality.Countable,
    T extends Po<C, T>
    >
    extends Set<T>, Comparable<T> {
    }
}
