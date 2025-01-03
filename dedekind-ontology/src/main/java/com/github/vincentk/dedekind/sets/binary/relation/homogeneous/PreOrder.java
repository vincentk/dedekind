package com.github.vincentk.dedekind.sets.binary.relation.homogeneous;

/**
 * @param <T>
 * 
 * Notably, a {@link PreOrder} is not required to be symmetric.
 * 
 * @see https://en.wikipedia.org/wiki/Preorder
 */
public interface PreOrder<T> {

    /**
     * @param that
     * @return true exactly if this <= that
     */
    boolean leq(T that);

    public interface Directed<T>
    extends PreOrder<T> {

	/**
	 * @param that
	 * @return an upper bound value F such that this <= F and that <= F
	 */
	T upperBound(T that);
    }
    
    /**
     * A {@link} which is guaranteed by the implementor to be anti-symmetric.
     * 
     * I.e. a <= b && b <= a => a == b
     * 
     * @param <T>
     * 
     * @see https://en.wikipedia.org/wiki/Partially_ordered_set#Partial_orders
     */
    public interface AntiSymmetric<T extends AntiSymmetric<T>>
    extends
    PreOrder<T>, Identity<T>
    {
	@SuppressWarnings("unchecked")
	@Override
	default boolean eq(T that) {
	    return leq(that) && that.leq((T) this);
	}
    }
}
