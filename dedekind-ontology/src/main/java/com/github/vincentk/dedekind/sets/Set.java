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
     * @param that
     * @return this &cap; that
     */
    Set<E, ?> intersection(Set<E, ?> that);
    
    /**
     * @param that
     * @return this &cup; that
     */
    Set<E, ?> union(Set<E, ?> that);
    
    /**
     * @param that
     * @return {x &isin; this | Φ(x)}
     */
    //Set<E, ?> where(Predicate<E> Φ);
    
    /**
     * @param that
     * @return this &sub; that
     */
    boolean sub(Set<E, ?> that);
    
    /**
     * @param that
     * @return this &sup; that
     */
    boolean sup(Set<E, ?> that);

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
