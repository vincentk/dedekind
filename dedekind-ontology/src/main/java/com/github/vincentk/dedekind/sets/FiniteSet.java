package com.github.vincentk.dedekind.sets;

/**
 * Finite sets have finite cardinality.
 * 
 * @see https://en.wikipedia.org/wiki/Finite_set
 */
public interface FiniteSet<
E extends Element<E>,
C extends Cardinality.Finite,
T extends FiniteSet<E, C, T>
>
extends
CountableSet<E, C, T>,
Cardinality.Finite
{
    public interface B64<
    E extends Element<E>,
    C extends Cardinality.Finite.PowerOfTwo.B63,
    T extends B64<E, C, T>
    >
    extends
    FiniteSet<E, C, T>,
    Cardinality.Finite.PowerOfTwo.B64
    {

    }
}