package com.github.vincentk.dedekind.sets.ordered;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Set;
import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.PreOrder;

/**
 * Set with a preorder and an upper bound &isin; set.
 * 
 * @param <C> cardinality
 * @param <T> implementation type
 * 
 * @see https://en.wikipedia.org/wiki/Directed_set
 */
public interface Directed<
C extends Cardinality,
T extends Directed<C, T>
>
extends Set<T>, PreOrder.Directed<T> {
}