package com.github.vincentk.dedekind.sets;

import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.Identity;

/**
 * 
 * A set.
 * 
 * @param <T> implementation type.
 * 
 * @see https://en.wikipedia.org/wiki/Set_(mathematics)
 */
public interface Set<
E extends Set.Element<E>,
T extends Set<E, T>>
extends
Identity<T>
{
    /**
     * By default, set membership is tested via a type-check.
     * 
     * @param elem
     * @return elem &isin; this
     */
    default boolean isin(E elem) {
	return !isEmpty();
    }

    /**
     * @return true exactly if this is &empty;.
     */
    boolean isEmpty();

    /**
     * An element of a set.
     */
    interface Element<E extends Element<E>>
    extends Identity<E>
    {
	@SuppressWarnings("unchecked")
	@Override
	default boolean eq(E that) {
	    return ((E) this).equals(that);
	}
    }
}
