package com.github.vincentk.dedekind.algebra.binary;

import com.github.vincentk.dedekind.algebra.unary.Monoid;
import com.github.vincentk.dedekind.algebra.unary.Ring;
import com.github.vincentk.dedekind.algebra.unary.SemiRing;
import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * Definition of scalar multiplication over a {@link Ring}.
 *
 * Strictly speaking, this is the definition of a "right semi-module"
 * defining multiplication by a scalar from the right.
 *
 * @see https://en.wikipedia.org/wiki/Semimodule
 */
public interface SemiModule<
R extends SemiRing<R>,
C extends Cardinality,
N extends SemiModule<R, C, N>>
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
