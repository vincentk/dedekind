package com.github.vincentk.dedekind.algebra.binary;

import com.github.vincentk.dedekind.algebra.Monoid;
import com.github.vincentk.dedekind.algebra.Ring;
import com.github.vincentk.dedekind.algebra.SemiRing;

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
public interface SemiModule<R extends SemiRing<R>, N extends SemiModule<R, N>>
extends Monoid.P<N>
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
    @Override
    N plus(N module);
}
