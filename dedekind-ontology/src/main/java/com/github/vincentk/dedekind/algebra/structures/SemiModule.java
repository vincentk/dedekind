package com.github.vincentk.dedekind.algebra.structures;

/**
 * Definition of scalar multiplication over a {@link SemiRing}.
 *
 * Strictly speaking, this is the definition of a "right semi-module"
 * defining multiplication by a scalar from the right.
 *
 * @see https://en.wikipedia.org/wiki/Semimodule
 *
 * @param <R> the scalar ring
 * @param <N> self-type
 */
public interface SemiModule<
R extends SemiRing.SmrE<R>,
E extends SemiModule.SmE<R, E>,
N extends SemiModule<R, E, N>>
extends Monoid.P<E, N>
{
    interface SmE<R extends SemiRing.SmrE<R>, E extends SmE<R, E>>
    extends Monoid.P.Pe<E>
    {
	/**
	 * Scalar multiplication.
	 *
	 * Expectations:
	 * https://en.wikipedia.org/wiki/Module_(mathematics)#Formal_definition
	 *
	 * @param scalar
	 * @return the scaled module element / vector.
	 */
	E mult(R scalar);

	/**
	 * Module addition.
	 *
	 * @param module from the same vector space.
	 * @return a new vector in the same vector space.
	 */
	@Override
	E plus(E module);	
    }
}
