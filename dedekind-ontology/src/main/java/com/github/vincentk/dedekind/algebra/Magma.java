package com.github.vincentk.dedekind.algebra;

import com.github.vincentk.dedekind.sets.Set;

/**
 * A set A with a binary operation (A, A) -> A.
 * 
 * Notably, java prohibits implementing interfaces twice with different generic parameters.
 * As a result, we need to distinguish statically between e.g. (M, +) and (M, *).
 * 
 * https://en.wikipedia.org/wiki/Magma_(algebra)
 * 
 * @param <E> element type
 * @param <T> implementation type
 */
public interface Magma<E, T extends Magma<E, T>> extends Set<E, T> {

    /**
     * Magma under addition (M, +).
     * 
     * @param <E> element type
     * @param <T> the implementing type.
     */
    interface P<E, T extends P<E, T>> extends Magma<E, T> {

        T plus(T that);

        // Poor man's operator overloading:
        default T p(T that) {
            return plus(that);
        }
    }
    
    /**
     * Magma under multiplication (M, *).
     * 
     * @param <E> element type
     * @param <T> the implementing type.
     */
    interface M<E, T extends M<E, T>> extends Magma<E, T> {

        T times(T that);

        // Poor man's operator overloading:
        default T x(T that) {
            return times(that);
        }
    }
}
