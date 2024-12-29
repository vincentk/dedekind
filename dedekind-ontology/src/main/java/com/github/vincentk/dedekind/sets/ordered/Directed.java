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
E extends Directed.De<E>,
C extends Cardinality,
T extends Directed<E, C, T>
>
extends Set<E, T> {
    
    interface De<E extends De<E>>
    extends
    Set.Element<E>, PreOrder.Directed<E>
    {
	
    }
}