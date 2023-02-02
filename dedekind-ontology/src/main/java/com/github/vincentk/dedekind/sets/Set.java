package com.github.vincentk.dedekind.sets;

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
     * @param <S> enumeration (e.g. a Supplier<T>, a Stream<T>, or an Iterable<T>).
     * @param <T> implementation type
     */
    interface Countable<
    C extends Cardinality.Countable,
    S,
    T extends Countable<C, S, T>
    >
    extends Set<T>{

        S enumerate();
    }

    /**
     * Finite sets have finite cardinality.
     * 
     * @param <S> enumeration (e.g. a Supplier<T>, a Stream<T>, or an Iterable<T>).
     * @param <T> implementation type
     */
    interface Finite<
    S,
    T extends Finite<S, T>
    >
    extends Countable<Cardinality.Finite, S, T>, Cardinality.Finite {

    }

    /**
     * Partially ordered set.
     * 
     * @param <C> cardinality
     * @param <S> enumeration (e.g. a Supplier<T>, a Stream<T>, or an Iterable<T>).
     * @param <T> implementation type
     */
    interface Po<
    C extends Cardinality.Countable,
    S,
    T extends Countable<C, S, T>
    >
    extends Set<T>, Comparable<T> {
    }
}
