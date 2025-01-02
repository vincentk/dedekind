package com.github.vincentk.dedekind.sets.ordered;

import com.github.vincentk.dedekind.graph.DirectedGraph;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Element;
import com.github.vincentk.dedekind.sets.NonEmptySet;
import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.Identity;
import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.PreOrder;

/**
 * A {@link NonEmptySet} with a {@link PreOrder}.
 * 
 * @param <C> cardinality
 * @param <T> implementation type
 * 
 * @see https://en.wikipedia.org/wiki/Preorder
 * @see https://en.wikipedia.org/wiki/Directed_graph
 */
public interface PreOrderedSet<
E extends PreOrderedSet.De<E>,
C extends Cardinality,
T extends PreOrderedSet<E, C, T>
>
extends
NonEmptySet<E, C, T>,
DirectedGraph<E>
{

    interface De<E extends De<E>>
    extends
    Element<E>, PreOrder<E>
    {
	/**
	 * A trivial {@link PreOrder} using the {@link Identity} relation.
	 * 
	 * <p>
	 * {@inheritDoc}
	 * </p>
	 */
	@SuppressWarnings("unchecked")
	@Override
	default boolean leq(E that) {
	    return ((E) this).eq(that);
	}
    }
}