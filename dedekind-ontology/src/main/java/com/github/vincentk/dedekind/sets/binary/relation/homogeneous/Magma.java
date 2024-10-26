package com.github.vincentk.dedekind.sets.binary.relation.homogeneous;

import com.github.vincentk.dedekind.sets.Set;
import com.github.vincentk.dedekind.sets.unary.function.operation.Operation;
import com.github.vincentk.dedekind.sets.unary.function.operation.arithmetic.Addition;
import com.github.vincentk.dedekind.sets.unary.function.operation.arithmetic.Multiplication;

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
    interface P<T extends P<T>> extends Magma<T>, Addition<T, T>, Operation<T, T> {

	@Override
	default T ap(T that) {
	    return plus(that);
	}
    }

    /**
     * Magma under multiplication (M, *).
     * 
     * @param <T> the implementing type.
     */
    interface M<T extends M<T>> extends Magma<T>, Multiplication<T, T>, Operation<T, T> {

	// Would lead to compilation errors due to duplicate default methods with
	// different type parameters:
	/**
	@Override
	default T ap(T that) {
	    return times(that);
	}
	 **/
    }
}
