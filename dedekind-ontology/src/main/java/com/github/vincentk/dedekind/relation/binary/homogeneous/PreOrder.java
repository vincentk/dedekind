package com.github.vincentk.dedekind.relation.binary.homogeneous;

/**
 * @param <T>
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
	 * 
	 * @see https://en.wikipedia.org/wiki/Preorder
	 */
	T upperBound(T that);
    }
}
