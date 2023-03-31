package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.Module;
import com.github.vincentk.dedekind.algebra.Monoid;
import com.github.vincentk.dedekind.algebra.SemiRing;

/**
 * An element of a vector space satisfying the vector axioms.
 *
 * Notably, this does not include e.g. the definition of an inner product.
 * For this, we refer to sub-types.
 * 
 * @see https://en.wikipedia.org/wiki/Vector_space#Definition_and_basic_properties
 *
 * @param <F> usually a {@link Field}.
 * @param <V> the usual recursive self-type so that subtypes can refer to themselves.
 */
public interface Vector<
// Field:
F extends SemiRing<F>,
// The dual:
D extends Vector<F, ?, ?>,
// Self-reference:
V extends Vector<F, D, V>
>
extends
// Vector addition:
Monoid.P<V>,
// Scalar multiplication:
Module<F, V>,
// Transpose operation:
Dual<D>
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
    
    @Override
    D transpose();
}
