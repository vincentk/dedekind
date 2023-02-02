package com.github.vincentk.dedekind.sets;

/**
 * 
 * A set. Membership is defined by delegating to the instanceof operation.
 * 
 * @param <E> element type
 * @param <T> implementation type.
 */
public interface Set<E, T extends Set<E, T>> {

    /**
     * A countable set. Its elements can be enumerated.
     * 
     * @param <E> element type
     * @param <C> cardinality
     * @param <S> enumeration (e.g. a Supplier<E>, a Stream<E>, or an Iterable<E>).
     * @param <T> implementation type
     */
    interface Countable<
    E,
    C extends Cardinality.Countable,
    S,
    T extends Countable<E, C, S, T>
    >
    extends Set<E, T>{

        S enumerate();
    }

    /**
     * Finite sets have finite cardinality.
     * 
     * @param <E> element type
     * @param <S> enumeration (e.g. a Supplier<E>, a Stream<E>, or an Iterable<E>).
     * @param <T> implementation type
     */
    interface Finite<
    E,
    S,
    T extends Finite<E, S, T>
    >
    extends Countable<E, Cardinality.Finite, S, T>, Cardinality.Finite {

    }
}
