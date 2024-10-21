package com.github.vincentk.dedekind.relation.binary.homogeneous;

import com.github.vincentk.dedekind.algebra.arithmetic.Addition;
import com.github.vincentk.dedekind.algebra.arithmetic.Multiplication;
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
    interface P<T extends P<T>> extends Magma<T>, Addition<T, T> {
    }
    
    /**
     * Magma under multiplication (M, *).
     * 
     * @param <T> the implementing type.
     */
    interface M<T extends M<T>> extends Magma<T>, Multiplication<T, T> {
    }
}
