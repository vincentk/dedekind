package com.github.vincentk.dedekind.algebra.structures;

import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * An associative {@link Magma}.
 * 
 * @see https://en.wikipedia.org/wiki/Semigroup
 * 
 * @param <T> implementation type
 */
public interface SemiGroup<
E extends SemiGroup.Oe<E>,
C extends Cardinality,
T extends SemiGroup<E, C, T>>
extends
Magma<E, C, T>
{
    interface Oe<E extends Oe<E>>
    extends
    Magma.Oe<E>
    {
    }
}
