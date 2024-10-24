package com.github.vincentk.dedekind.sets.binary.relation;

import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.Monoid;
import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.SemiRing;

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
