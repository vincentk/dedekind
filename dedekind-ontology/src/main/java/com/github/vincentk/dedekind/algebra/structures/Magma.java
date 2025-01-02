package com.github.vincentk.dedekind.algebra.structures;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Element;
import com.github.vincentk.dedekind.sets.NonEmptySet;
import com.github.vincentk.dedekind.sets.binary.function.arithmetic.Addition;
import com.github.vincentk.dedekind.sets.binary.function.arithmetic.Multiplication;
import com.github.vincentk.dedekind.sets.binary.function.operation.BinaryOperation;

/**
 * A set A with a binary operation (A, A) -> A.
 * 
 * Notably, java prohibits implementing interfaces twice with different generic parameters.
 * As a result, we need to distinguish statically between e.g. (M, +) and (M, *).
 * 
 * @see https://en.wikipedia.org/wiki/Magma_(algebra)
 * 
 * @param <T> implementation type
 */
public interface Magma<
E extends Magma.Oe<E>,
C extends Cardinality,
T extends Magma<E, C, T>>
extends
NonEmptySet<E, C, T>
{
    interface Oe<E extends Oe<E>>
    extends
    Element<E>,
    BinaryOperation<E, E>
    {
    }

    /**
     * Magma under addition (M, +).
     * 
     * @param <T> the implementing type.
     */
    interface P<
    E extends P.Pe<E>,
    C extends Cardinality,
    T extends P<E, C, T>
    >
    extends
    Magma<E, C, T>
    {
	/**
	 * Elements of the magma.
	 * 
	 * @param <E>
	 */
	interface Pe<E extends Pe<E>>
	extends
	Oe<E>, Addition<E, E>
	{
	    @Override
	    default E ap(E that) {
		return plus(that);
	    }	    
	}
    }

    /**
     * Magma under multiplication (M, *).
     * 
     * @param <T> the implementing type.
     */
    interface M<
    E extends M.Me<E>,
    C extends Cardinality,
    T extends M<E, C, T>
    >
    extends
    Magma<E, C, T>
    {
	/**
	 * Elements of the magma.
	 * 
	 * @param <E>
	 */
	interface Me<E extends Me<E>>
	extends
	Oe<E>, Multiplication<E, E>
	{
	    @Override
	    default E ap(E that) {
		return times(that);
	    }	    
	}
    }
}
