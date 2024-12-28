package com.github.vincentk.dedekind.sets.ordered;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Set;
import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.PartialOrder;

/**
 * Partially ordered set.
 * 
 * @param <C>
 * @param <T>
 * 
 * @see https://en.wikipedia.org/wiki/Partially_ordered_set
 */
public interface PoSet<
E extends PoSet.Pe<E>,
C extends Cardinality,
T extends PoSet<E, C, T>
>
extends Set<E, T>
{
    interface Pe<E extends Pe<E>>
    extends
    Set.Element<E>, PartialOrder.Strict<E>
    {
	@Override
	default boolean eq(E that) {
	    return Set.Element.super.eq(that);
	}	
    }
}