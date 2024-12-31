package com.github.vincentk.dedekind.sets;

import com.github.vincentk.dedekind.algebra.numbers.N.Nat;
import com.github.vincentk.dedekind.families.Sequence;
import com.github.vincentk.dedekind.sets.unary.function.Lambda;

/**
 * A {@link Countable} set. Its elements can be enumerated.
 * 
 * @param <C> cardinality
 * @param <T> implementation type
 */
public interface Countable<
E extends Element<E>,
C extends Cardinality.Countable,
T extends Countable<E, C, T>
>
extends
Set<E, T>
{
    /**
     * @param enumeration
     * @return
     * 
     * @see https://en.wikipedia.org/wiki/Enumeration#Set_theory
     * 
     * @param <D> natural numbers
     */
    <D extends Nat<D>>
    Sequence<E, C, D, ?, ?>
    enumerate(Lambda<D, E, ?> enumeration);
}