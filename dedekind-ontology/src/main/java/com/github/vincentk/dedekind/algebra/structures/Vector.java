package com.github.vincentk.dedekind.algebra.structures;

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
 */
public interface Vector<
E extends Field.Fe<E>,
C extends Cardinality,
F extends Vector.Ve<E, F>,
// Self-reference:
V extends Vector<E, C, F, V>
>
extends
Module<E, F, C, V>
{
    interface Ve<R extends Ring.SmrE<R>, E extends Me<R, E>>
    extends Module.Me<R, E>
    {	
    }
}
