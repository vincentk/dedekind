package com.github.vincentk.dedekind.algebra.binary;

import com.github.vincentk.dedekind.algebra.Field;
import com.github.vincentk.dedekind.algebra.Ring;
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
F extends Ring<F>,
// Dimension of the vector space:
C extends Cardinality,
// Self-reference:
V extends Vector<F, C, V>
>
extends
Module<F, C, V>
{
}
