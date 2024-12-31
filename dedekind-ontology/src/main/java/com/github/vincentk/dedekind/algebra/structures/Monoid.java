package com.github.vincentk.dedekind.algebra.structures;

import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * A {@link Magma} with the following properties:
 * 
 * a) existence of a unit/identity element,
 * 
 * b) the operation is associative, e.g. (a + b) + c = a + (b + c).
 * 
 * @see https://en.wikipedia.org/wiki/Identity_element
 * @see https://en.wikipedia.org/wiki/Monoid
 * 
 * @param <T> the implementation type
 */
public interface Monoid<
E extends Monoid.Me<E>,
C extends Cardinality,
T extends Monoid<E, C, T>
>
extends SemiGroup<E, C, T>
{
    /**
     * Set elements of a monoid.
     * 
     * @param <E>
     */
    interface Me<E extends Me<E>>
    extends SemiGroup.Oe<E>
    {
	boolean isIdentity();
    }

    /**
     * Monoid under addition (M, +).
     * 
     * @param <T> the implementing type.
     */
    interface P<
    E extends Monoid.P.Pe<E>,
    C extends Cardinality,
    T extends P<E, C, T>>
    extends Monoid<E, C, T>, Magma.P<E, C, T>
    {
	interface Pe<E extends Pe<E>>
	extends
	Magma.P.Pe<E>,
	Monoid.Me<E>
	{

	    default @Override
	    boolean isIdentity() {
		return isIdentityP();
	    }

	    /**
	     * E.g. for numbers, 0 is the identity element for addition, as
	     * x + 0 = x for all x.
	     * 
	     * Note, this default implementation will not work for
	     * boolean algebra (with + being ||), 
	     * as x || x = x for all boolean values.
	     * I.e.
	     * true  || true  = true
	     * false || false = false
	     * 
	     * @return true exactly if this is the identity element for addition.
	     */
	    @SuppressWarnings("unchecked")
	    default boolean isIdentityP() {
		// x + x = x
		// =>
		// x = 0
		// i.e.
		// 0 + 0 = 0
		return eq(plus((E) this));
	    }
	}
    }

    /**
     * Monoid under multiplication (M, *).
     * 
     * @param <T> the implementing type.
     */
    interface M<
    E extends Monoid.M.Te<E>,
    C extends Cardinality,
    T extends M<E, C, T>> 
    extends
    Monoid<E, C, T>,
    Magma.M<E, C, T>
    {

	interface Te<E extends Te<E>>
	extends
	Magma.M.Me<E>,
	Monoid.Me<E>
	{

	    default @Override
	    boolean isIdentity() {
		return isIdentityM();
	    }

	    /**
	     * E.g. for numbers, 1 is the unit element for multiplication, as
	     * x * 1 = x for all x.
	     * 
	     * @param val
	     * @return true exactly if this is the unit element for multiplication.
	     */
	    boolean isIdentityM();
	}
    }
}
