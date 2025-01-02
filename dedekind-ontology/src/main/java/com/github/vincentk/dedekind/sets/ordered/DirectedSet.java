package com.github.vincentk.dedekind.sets.ordered;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Element;
import com.github.vincentk.dedekind.sets.NonEmptySet;
import com.github.vincentk.dedekind.sets.Set;
import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.Identity;
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
E extends DirectedSet.De<E>,
C extends Cardinality,
T extends DirectedSet<E, C, T>
>
extends NonEmptySet<E, C, T> {

    interface De<E extends De<E>>
    extends
    Element<E>, PreOrder.Directed<E>
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