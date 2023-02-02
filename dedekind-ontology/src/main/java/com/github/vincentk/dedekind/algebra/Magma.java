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
 * @param <T> implementation type
 */
public interface Magma<T extends Magma<T>> extends Set<T> {

    /**
     * Magma under addition (M, +).
     * 
     * @param <T> the implementing type.
     */
    interface P<T extends P<T>> extends Magma<T> {

        T plus(T that);

        // Poor man's operator overloading:
        default T p(T that) {
            return plus(that);
        }
    }
    
    /**
     * Magma under multiplication (M, *).
     * 
     * @param <T> the implementing type.
     */
    interface M<T extends M<T>> extends Magma<T> {

        T times(T that);

        // Poor man's operator overloading:
        default T x(T that) {
            return times(that);
        }
    }
}
