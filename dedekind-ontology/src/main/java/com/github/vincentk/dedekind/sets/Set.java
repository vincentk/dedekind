package com.github.vincentk.dedekind.sets;

import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.Identity;

/**
 * 
 * A set.
 * 
 * Membership is defined by delegating to the instanceof operation
 * of implementing classes.
 * 
 * @param <T> implementation type.
 * 
 * @see https://en.wikipedia.org/wiki/Set_(mathematics)
 */
public interface Set<
E extends Set.Element<? extends E>,
T extends Set<E, T>>
extends
Identity<T>
{
    @Deprecated
    @SuppressWarnings("unchecked")
    @Override
    default boolean eq(T that) {
	return ((T) this).equals(that);
    }

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
