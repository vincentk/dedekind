package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.Field;
import com.github.vincentk.dedekind.algebra.MonoidP;

/**
 * A vector space in the spirit of the 
 * <a href="https://en.wikipedia.org/wiki/Vector_space#Definition">definition found on wikipedia</a>.
 * @param <F> optional type tag indicating e.g. a {@link Field}.
 * @param <V> the usual recursive self-type so that subtypes can refer to themselves.
 */
public interface Vector<F, V extends Vector<F, V>>
extends MonoidP<V>
{

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
     * Monoid laws for {@link MonoidP}
     * v + v = 2 v .
     * v + 0 = v .
     *
     * @param vector a vector from the same vector space.
     * @return a new vector in the same vector space.
     */
    @Override
    V plus(V vector);
}
