package com.github.vincentk.dedekind.algebra;

import com.github.vincentk.dedekind.sets.Rings;

public interface Ring<R extends Ring<R>> extends Rings, MonoidP<R>, MonoidM<R> {

	/**
	 * The integers under multiplication form a
	 * <a href="https://en.wikipedia.org/wiki/Commutative_ring">Commutative Ring</a>.
	 */
	interface Integer<I extends Integer<I>> extends Ring<I>, Integers {}

	/**
	 * Naturals are a sub-set of the integers.
	 */
	interface Natural<N extends Natural<N>> extends Integer<N>, Naturals {}
}
