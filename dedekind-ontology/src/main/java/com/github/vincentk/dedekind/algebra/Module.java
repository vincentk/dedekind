package com.github.vincentk.dedekind.algebra;


/**
 * Definition of scalar multiplication over a {@link Ring}.
 *
 * Strictly speaking, this is the definition of a "right module"
 * defining multiplication by a scalar from the right.
 *
 * @see https://en.wikipedia.org/wiki/Module_(mathematics)
 *
 * @param <R> the scalar ring
 * @param <M> self-type
 */
public interface Module<R extends SemiRing<R>, N extends Module<R, N>>
extends Group.P<N>
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
    N mult(R scalar);

    /**
     * Module addition.
     *
     * @param module from the same vector space.
     * @return a new vector in the same vector space.
     */
    N plus(N module);
}
