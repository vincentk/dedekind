package com.github.vincentk.dedekind.sets.ordered;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Set;
import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.PreOrder;

/**
 * A non-empty {@link Set} with a preorder and an upper bound &isin; set.
 * 
 * @param <C> cardinality
 * @param <T> implementation type
 * 
 * @see https://en.wikipedia.org/wiki/Directed_set
 */
public interface DirectedSet<
E extends PreOrderedSet.De<E>,
C extends Cardinality,
T extends DirectedSet<E, C, T>
>
extends
PreOrderedSet<E, C, T>
{
    interface De<E extends De<E>>
    extends
    PreOrderedSet.De<E>, PreOrder.Directed<E>
    {
    }
}