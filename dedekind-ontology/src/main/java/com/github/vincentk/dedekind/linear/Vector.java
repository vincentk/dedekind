package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.Field;

/**
 * A vector space in the spirit of the 
 * <a href="https://en.wikipedia.org/wiki/Vector_space#Definition">definition found on wikipedia</a>.
 * @param <F> optional type tag indicating e.g. a {@link Field}.
 * @param <V> the usual recursive self-type so that subtypes can refer to themselves.
 */
public interface Vector<F, V extends Vector<F, V>> {

    /**
     * Scalar multiplication.
     *
     * @param scalar a scalar from the underlying field.
     * @return the scaled vector.
     */
    V scalarMult(F scalar);

    /**
     * Vector addition.
     *
     * @param vector a vector from the same vector space.
     * @return a new vector in the same vector space.
     */
    V vAdd(V vector);
}
