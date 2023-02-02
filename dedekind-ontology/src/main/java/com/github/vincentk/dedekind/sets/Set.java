package com.github.vincentk.dedekind.sets;

/**
 * A countable set. Its elements can be enumerated.
 * 
 * @param <E> element type
 * @param <S> enumeration (e.g. a Supplier<E>, or an Iterable<E>).
 */
public interface Set {
    
    interface Countable<E, C extends Cardinality.Countable, S> {

        S enumerate();
    }

    /**
     * Finite sets have finite cardinality.
     * 
     * @param <E>
     * @param <S>
     */
    interface Finite<E, S> extends Countable<E, Cardinality.Finite, S>, Cardinality.Finite {

    }
}
