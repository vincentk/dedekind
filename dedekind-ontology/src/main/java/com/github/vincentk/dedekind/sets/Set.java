package com.github.vincentk.dedekind.sets;

import java.util.function.Predicate;

import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.Identity;

/**
 * 
 * A set.
 * 
 * @param <T> implementation type.
 * 
 * @see https://en.wikipedia.org/wiki/Set_(mathematics)
 */
public
interface Set<
E extends Element<E>,
T extends Set<E, T>>
extends
Identity<Set<E, ?>>
{
    @Override
    default boolean eq(Set<E, ?> that) {
	return sub(that) && sup(that);
    }

    /**
     * By default, set membership is tested via a type-check.
     * 
     * @param elem
     * @return elem &isin; this
     */
    default boolean contains(E elem) {
	return !isEmpty();
    }

    /**
     * @return true exactly if this is &empty;.
     */
    default boolean isEmpty() {
	return this instanceof EmptySet<?>;
    }

    /**
     * @param that
     * @return this &cap; that
     */
    default Set<E, ?> intersection(Set<E, ?> that) {
	return where(x -> that.contains(x));
    }

    /**
     * @param that
     * @return this &cup; that
     */
    default Set<E, ?> union(Set<E, ?> that) {

	if (isEmpty()) return that;

	if (that.isEmpty()) return this;

	final var cpl = complement(that);
	if (cpl.isEmpty()) return that;

	return new Union<E>(this, cpl);
    }

    /**
     * @param that
     * @return {x &isin; this | Φ(x)}
     * 
     * @see https://en.wikipedia.org/wiki/Set-builder_notation
     */
    Set<E, ?> where(Predicate<E> Φ);

    /**
     * The relative complement (a.k.a left difference) of this vs. that.
     * 
     * @param that
     * @return {x &isin; this | &not; x &isin; that}
     */
    default Set<E, ?> complement(Set<E, ?> that) {
	return where(x -> !that.contains(x));
    }

    /**
     * @param that
     * @return this &sub; that
     */
    default boolean sub(Set<E, ?> that) {
	return complement(that).isEmpty();
    }

    /**
     * @param that
     * @return this &sup; that
     */
    default boolean sup(Set<E, ?> that) {
	return that.complement(this).isEmpty();
    }
}
