package com.github.vincentk.dedekind.sets.binary.relation.homogeneous;

/**
 * A {@link Magma} with the following properties:
 * 
 * a) existence of a unit/identity element,
 * 
 * b) the operation is associative, e.g. (a + b) + c = a + (b + c).
 * 
 * @see https://en.wikipedia.org/wiki/Monoid
 * 
 * @param <T> the implementation type
 */
public interface Monoid<T extends Monoid<T>> extends Magma<T> {

    /**
     * Monoid under addition (M, +).
     * 
     * @param <T> the implementing type.
     */
    interface P<T extends P<T>> extends Monoid<T>, Magma.P<T> {

	/**
	 * E.g. for numbers, 0 is the identity element for addition, as
	 * x + 0 = x for all x.
	 * 
	 * @return true exactly if this is the identity element for addition.
	 */
	@SuppressWarnings("unchecked")
	default boolean isIdentityP() {
	    // 0 + 0 = 0
	    return eq(plus((T) this));
	}
    }

    /**
     * Monoid under multiplication (M, *).
     * 
     * @param <T> the implementing type.
     */
    interface M<T extends M<T>> extends Monoid<T>, Magma.M<T> {

	/**
	 * E.g. for numbers, 1 is the unit element for multiplication, as
	 * x * 1 = x for all x.
	 * 
	 * @param val
	 * @return true exactly if this is the unit element for multiplication.
	 */
	@SuppressWarnings("unchecked")
	default boolean isIdentityM() {
	    // 1 * 1 = 1
	    return eq(times((T) this));
	}
    }
}
