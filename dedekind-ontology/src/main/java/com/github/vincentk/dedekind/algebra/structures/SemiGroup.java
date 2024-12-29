package com.github.vincentk.dedekind.algebra.structures;

/**
 * An associative {@link Magma}.
 * 
 * @see https://en.wikipedia.org/wiki/Semigroup
 * 
 * @param <T> implementation type
 */
public interface SemiGroup<
E extends SemiGroup.Oe<E>,
T extends SemiGroup<E, T>>
extends
Magma<E, T>
{
    interface Oe<E extends Oe<E>>
    extends
    Magma.Oe<E>
    {
    }
}
