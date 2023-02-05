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
public interface Module<R extends SemiRing<R>, M extends Module<R, M>> {

    /**
     * Scalar multiplication.
     *
     * Expectations:
     * https://en.wikipedia.org/wiki/Module_(mathematics)#Formal_definition
     *
     * @param scalar
     * @return the scaled module element / vector.
     */
    M mult(R scalar);
}
