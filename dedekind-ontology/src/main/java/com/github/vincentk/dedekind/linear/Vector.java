package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.Field;
import com.github.vincentk.dedekind.algebra.MonoidP;
import com.github.vincentk.dedekind.algebra.Ring;

/**
 * An element of a vector space satisfying the
 * <a href="https://en.wikipedia.org/wiki/Vector_space#Definition_and_basic_properties">vector axioms</a>.
 * 
 * Notably, this does not include e.g. the definition of an inner product.
 * For this, we refer to sub-types.
 * 
 * @param <F> optional type tag indicating e.g. a {@link Field}.
 * @param <V> the usual recursive self-type so that subtypes can refer to themselves.
 */
public interface Vector<F extends Ring<F>, V extends Vector<F, V>>
extends
// Vector addition:
MonoidP<V>,
// Scalar multiplication:
Module<F, V>
{

    /**
     * Scalar multiplication.
     *
     * @param scalar a scalar from the underlying field.
     * @return the scaled vector.
     */
	@Override
    V mult(F scalar);

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
