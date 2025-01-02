package com.github.vincentk.dedekind.sets.ordered;

import com.github.vincentk.dedekind.graph.DirectedAcyclicGraph;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Element;
import com.github.vincentk.dedekind.sets.NonEmptySet;
import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.PartialOrder;

/**
 * Partially ordered set.
 * 
 * @param <C>
 * @param <T>
 * 
 * @see https://en.wikipedia.org/wiki/Partially_ordered_set
 * @see https://en.wikipedia.org/wiki/Directed_acyclic_graph
 * @see https://math.stackexchange.com/q/1748204
 */
public interface PoSet<
E extends PoSet.Pe<E>,
C extends Cardinality,
T extends PoSet<E, C, T>
>
extends
NonEmptySet<E, C, T>,
DirectedAcyclicGraph<E>
{
    interface Pe<E extends Pe<E>>
    extends
    Element<E>, PartialOrder.Strict<E>
    {
	@Override
	default boolean eq(E that) {
	    return Element.super.eq(that);
	}	
    }
}