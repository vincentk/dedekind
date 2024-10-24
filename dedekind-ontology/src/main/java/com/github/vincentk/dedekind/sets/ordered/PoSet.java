package com.github.vincentk.dedekind.sets.ordered;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Set;
import com.github.vincentk.dedekind.sets.relation.binary.homogeneous.PartialOrder;

/**
 * Partially ordered set.
 * 
 * @param <C>
 * @param <T>
 * 
 * @see https://en.wikipedia.org/wiki/Partially_ordered_set
 */
public interface PoSet<
C extends Cardinality,
T extends PoSet<C, T>
>
extends Set<T>, PartialOrder.Strict<T> {
    @Override
    default boolean eq(T that) {
        return Set.super.eq(that);
    }
}