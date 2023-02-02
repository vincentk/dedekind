package com.github.vincentk.dedekind.algebra;

/**
 * A {@link Magma} with the following properties:
 * 
 * a) existence of a unit/identity element,
 * 
 * b) the operation is associative, e.g. (a + b) + c = a + (b + c).
 * 
 * @see https://en.wikipedia.org/wiki/Monoid
 * 
 * @param <E> element type
 * @param <T> the implementation type
 */
public interface Monoid<E, T extends Monoid<E, T>> extends Magma<E, T> {

    /**
     * @param val
     * @return true exactly if this is the unit element.
     */
    boolean isUnit();

    /**
     * Monoid under addition (M, +).
     * 
     * @param <T> the implementing type.
     */
    interface P<E, T extends P<E, T>> extends Monoid<E, T>, Magma.P<E, T> {
    }
    
    /**
     * Monoid under multiplication (M, *).
     * 
     * @param <T> the implementing type.
     */
    interface M<E, T extends M<E, T>> extends Monoid<E, T>, Magma.M<E, T>{
    }
}
