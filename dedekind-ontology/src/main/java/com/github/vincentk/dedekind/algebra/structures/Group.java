package com.github.vincentk.dedekind.algebra.structures;

import java.util.Optional;

/**
 * A {@link Monoid} with the following additional properties:
 * 
 * a) existence of an inverse operation such that x * x' = 1.
 *
 * @see https://en.wikipedia.org/wiki/Group_(mathematics)
 * 
 * @param <T> the implementation type
 */
public interface Group<
E extends Group.Ge<E>,
T extends Group<E, T>>
extends
Monoid<E, T> {

    /**
     * Set elements of a group.
     * 
     * @param <E>
     */
    interface Ge<E extends Ge<E>>
    extends Monoid.Me<E>
    {
	/**
	 * Notably, inversion is a partial function.
	 * 
	 * Division by zero and similar are typically undefined.
	 * 
	 * @return the inverse, if it exists.
	 */
	Optional<E> inverse();
    }

    /**
     * Group under addition (M, +, -).
     * 
     * @param <T> the implementing type.
     */
    interface P<
    E extends P.Pe<E>,
    T extends P<E, T>>
    extends Group<E, T>, Monoid.P<E, T> {

	/**
	 * Set elements of a group under addition.
	 * 
	 * @param <E>
	 */
	interface Pe<E extends Pe<E>>
	extends Group.Ge<E>, Monoid.P.Pe<E>
	{
	    /**
	     * @return the additive inverse.
	     */
	    default Optional<E> inverse() {
		return Optional.of(negate());
	    }

	    /**
	     * @return - this
	     */
	    E negate();

	    default E neg() {
		return negate();
	    }

	    default E minus(E that) {
		return plus(that.neg());
	    }
	}
    }

    /**
     * Group under multiplication (M, *, /).
     * 
     * Notably, inversion is a partial function.
     * 
     * Division by zero and similar are typically undefined.
     * 
     * @param <T> the implementing type.
     */
    interface M<
    E extends M.Me<E>,
    T extends M<E, T>>
    extends Group<E, T>,
    Monoid.M<E, T> {

	/**
	 * Set elements of a group under multiplication.
	 * 
	 * @param <E>
	 */
	interface Me<E extends Me<E>>
	extends Group.Ge<E>, Monoid.M.Te<E>
	{
	    default Optional<E> inverse() {
		return Optional.empty();
	    }

	    default Optional<E> inv() {
		return inverse();
	    }

	    default Optional<E> divide(E that) {
		return that.inv().map(this::times);
	    }
	}


	/**
	 * Group under multiplication (M, *, /).
	 * 
	 * The implementor guarantees at or before construction time that
	 * inversion is a total function (e.g. by ensuring that a zero element
	 * can not be constructed).
	 * 
	 * E.g. we are looking at "the reals without zero", 
	 * "the set of invertible matrices" or similar.
	 * 
	 * @param <T> the implementing type.
	 */
	interface M0<
	E extends Group.M.M0.Me0<E>,
	T extends M0<E, T>>
	extends Group.M<E, T>
	{
	    interface Me0<E extends Me0<E>>
	    extends Group.M.Me<E>, Monoid.M.Te<E>
	    {

		E inverse0();

		default E inv0() {
		    return inverse0();
		}

		default E divide0(E that) {
		    return times(that.inv0());
		}

		@Override
		default Optional<E> inverse() {
		    return Optional.of(inverse0());
		}

		@Override
		default Optional<E> divide(E that) {
		    return Optional.of(divide0(that));
		}
	    }
	}
    }
}
