package com.github.vincentk.dedekind.algebra.structures;

import java.util.Optional;

import com.github.vincentk.dedekind.algebra.sets.Fields;

/**
 * @see https://en.wikipedia.org/wiki/Field_(mathematics)#Examples
 */
public interface Field<
E extends Field.Fe<E>,
F extends Field<E, F>>
extends
Ring<E, F>, Group.M<E, F>
{
    interface Fe<R extends Fe<R>>
    extends
    Ring.Re<R>,
    Group.M.Me<R>
    {
	@Override
	default Optional<R> inverse() {
	    return Group.M.Me.super.inverse();
	}
    }
    /**
     * Complex numbers:
     */
    interface Complex<
    E extends Complex.Ce<E>,
    C extends Complex<E, C>
    >
    extends
    Field<E, C>
    {
	interface Ce<E extends Ce<E>>
	extends Fe<E>
	{
	    /**
	     * @return the complex conjugate
	     */
	    E conjugate();

	    default E conj() {
		return conjugate();
	    }
	}
    }

    /**
     * Dual numbers:
     */
    interface Duals<
    E extends Field.Fe<E>,
    R extends Duals<E, R>
    >
    extends
    Field<E, R> {}

    /**
     * Real numbers:
     */
    interface Reals<
    E extends Field.Fe<E>,
    R extends Reals<E, R>
    >
    extends
    Field<E, R>, Fields.Reals
    {}

    /**
     * Rational numbers:
     */
    interface Rationals<
    E extends Field.Fe<E>,
    Q extends Rationals<E, Q>
    >
    extends
    Field<E, Q>,
    Fields.Rationals
    {}


}
