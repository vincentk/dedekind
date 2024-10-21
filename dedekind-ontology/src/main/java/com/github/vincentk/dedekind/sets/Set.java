package com.github.vincentk.dedekind.sets;

import com.github.vincentk.dedekind.relation.binary.homogeneous.Equality;
import com.github.vincentk.dedekind.relation.binary.homogeneous.PartialOrder;
import com.github.vincentk.dedekind.relation.binary.homogeneous.PreOrder;
import com.github.vincentk.dedekind.relation.binary.homogeneous.TotalOrder;

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
extends Equality<T>
{
    @SuppressWarnings("unchecked")
    @Override
    default boolean eq(T that) {
	return ((T) this).equals(that);
    }

    /**
     * A countable set. Its elements can be enumerated.
     * 
     * @param <C> cardinality
     * @param <T> implementation type
     */
    interface Countable<
    C extends Cardinality.Countable,
    T extends Countable<C, T>
    >
    extends Set<T> {
    }

    /**
     * Finite sets have finite cardinality.
     * They are always countable (we grant the axiom of choice).
     * 
     * @param <T> implementation type
     */
    interface Finite<T extends Finite<T>>
    extends Countable<Cardinality.Finite, T>, Cardinality.Finite {
    }
    
    /**
     * Set with a preorder and an upper bound &isin; set.
     * 
     * @param <C> cardinality
     * @param <T> implementation type
     * 
     * @see https://en.wikipedia.org/wiki/Directed_set
     */
    interface Directed<
    C extends Cardinality,
    T extends Directed<C, T>
    >
    extends Set<T>, PreOrder<T> {
	
	/**
	 * @param that
	 * @return an upper bound value F such that this <= F and that <= F
	 * 
	 * @see https://en.wikipedia.org/wiki/Preorder
	 */
	T upperBound(T that);
    }
    
    interface PoSet<
    C extends Cardinality,
    T extends PoSet<C, T>
    >
    extends Set<T>, PartialOrder.Strict<T> {
	@Override
	default boolean eq(T that) {
	    return Set.super.eq(that);
	}
    }

    /**
     * Set with a total order.
     * 
     * @param <C> cardinality
     * @param <T> implementation type
     * 
     * @see https://en.wikipedia.org/wiki/Partially_ordered_set#Derived_notions
     */
    interface TotallyOrdered<
    C extends Cardinality,
    T extends TotallyOrdered<C, T>
    >
    extends Directed<C, T>, PoSet<C, T>, TotalOrder<T> {
	
	@Override
	default boolean leq(T that) {
	    return this.compareTo(that) <= 0;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	default T upperBound(T that) {
	    return this.compareTo(that) <= 0 ? (T) this : that;
	}
    }
}
