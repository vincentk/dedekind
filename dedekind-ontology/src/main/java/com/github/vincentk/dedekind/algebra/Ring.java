package com.github.vincentk.dedekind.algebra;

/**
 * Marker type denoting a <a href="https://en.wikipedia.org/wiki/Ring_(mathematics)">ring</a>.
 */
public interface Ring {

	/**
	 * The integers under multiplication form a
	 * <a href="https://en.wikipedia.org/wiki/Commutative_ring">Commutative Ring</a>.
	 */
	interface Integers extends Ring {

		long asLong();
	}

	/**
	 * Naturals are a sub-set of the integers.
	 */
	interface Naturals extends Integers {}
}
