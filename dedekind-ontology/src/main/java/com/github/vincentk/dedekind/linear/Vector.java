package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.Module;
import com.github.vincentk.dedekind.algebra.Monoid;
import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * An element of a vector space satisfying the vector axioms.
 *
 * Notably, this does not include e.g. the definition of an inner product.
 * For this, we refer to sub-types.
 * 
 * @see https://en.wikipedia.org/wiki/Vector_space#Definition_and_basic_properties
 * @see https://en.wikipedia.org/wiki/Dimension_(vector_space)
 *
 * @param <F> usually a {@link Field}.
 * @param <V> the usual recursive self-type so that subtypes can refer to themselves.
 */
public interface Vector<
// Field:
F extends SemiRing<F>,
// Dimension of the vector space:
C extends Cardinality,
// Self-reference:
V extends Vector<F, C, V>
>
extends
// Vector addition:
Monoid.P<V>,
// Scalar multiplication:
Module<F, V>
{

    /**
     * Scalar multiplication.
     *
     * @param scalar from the underlying field.
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
     * @param vector from the same vector space.
     * @return a new vector in the same vector space.
     */
    @Override
    V plus(V vector);
}
