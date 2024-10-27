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
public interface Set<T extends Set<T>>
extends Identity<T>
{
    @SuppressWarnings("unchecked")
    @Override
    default boolean eq(T that) {
	return ((T) this).equals(that);
    }
}
