package com.github.vincentk.dedekind.sets;

import com.github.vincentk.dedekind.families.Sequence;

/**
 * A {@link CountableSet}. Its elements can be enumerated.
 * 
 * @see https://en.wikipedia.org/wiki/Countable_set
 */
public interface CountableSet<
E extends Element<E>,
C extends Cardinality.Countable,
T extends CountableSet<E, C, T>
>
extends
Set<E, C, T>
{
    /**
     * A set is countable exactly if its elements can be arranged in a {@link Sequence}
     * such that every element is listed.
     * 
     * @return the corresponding {@link Sequence}
     * 
     * @see https://en.wikipedia.org/wiki/Enumeration#Set_theory
     * @see https://en.wikipedia.org/wiki/Countable_set#Definition
     */
    Sequence<?, ?, ?, ?, ?, ?>
    enumerate();
}