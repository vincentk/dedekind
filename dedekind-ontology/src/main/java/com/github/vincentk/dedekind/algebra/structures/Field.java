package com.github.vincentk.dedekind.algebra.structures;

import java.util.Optional;

import com.github.vincentk.dedekind.algebra.sets.Fields;
import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * @see https://en.wikipedia.org/wiki/Field_(mathematics)#Examples
 */
public interface Field<
E extends Field.Fe<E>,
C extends Cardinality,
F extends Field<E, C, F>>
extends
Ring<E, C, F>, Group.M<E, C, F>
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
    Z extends Cardinality,
    C extends Complex<E, Z, C>
    >
    extends
    Field<E, Z, C>
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
    C extends Cardinality,
    R extends Duals<E, C, R>
    >
    extends
    Field<E, C, R> {}

    /**
     * Real numbers:
     */
    interface Reals<
    E extends Field.Fe<E>,
    C extends Cardinality,
    R extends Reals<E, C, R>
    >
    extends
    Field<E, C, R>, Fields.Reals
    {}

    /**
     * Rational numbers:
     */
    interface Rationals<
    E extends Field.Fe<E>,
    C extends Cardinality.Countable,
    Q extends Rationals<E, C, Q>
    >
    extends
    Field<E, C, Q>,
    Fields.Rationals
    {}


}
