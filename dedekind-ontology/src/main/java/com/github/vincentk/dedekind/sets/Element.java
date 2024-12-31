package com.github.vincentk.dedekind.sets;

import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.Identity;

/**
 * An {@link Element} of a {@link Set}.
 * 
 * @see https://en.wikipedia.org/wiki/Element_(mathematics)
 */
public interface Element<E extends Element<E>>
extends Identity<E>
{
    @SuppressWarnings("unchecked")
    @Override
    default boolean eq(E that) {
	return ((E) this).equals(that);
    }

    @SuppressWarnings("unchecked")
    default boolean isin(Set<? super E, ?> set) {
	return set.contains((E) this);
    }
}