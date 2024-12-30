package com.github.vincentk.dedekind.sets.binary.relation.homogeneous;

/**
 * A {@link PartialOrder} is an anti-symmetric {@link PreOrder}.
 * I.e. a {@link PreOrder} which prohibits non-trivial cycles.
 * 
 * @see https://en.wikipedia.org/wiki/Partially_ordered_set
 */
public interface PartialOrder<T extends PartialOrder<T>>
extends
PreOrder.AntiSymmetric<T> {

    /**
     * A partial order where there exist no cycles.
     * 
     * @param <T>
     * 
     * @see https://en.wikipedia.org/wiki/Partially_ordered_set#Strict_partial_orders
     */
    public interface Strict<T extends PartialOrder<T>>
    extends PartialOrder<T> {

	/**
	 * @param that
	 * @return true exactly if this < that
	 */
	default boolean lt(T that) {
	    return leq(that) && !eq(that);
	}
    }
}
