package com.github.vincentk.dedekind.algebra;

import com.github.vincentk.dedekind.sets.Rings;

/**
 * @param <R>
 */
public interface Ring<R extends Ring<R>> extends Rings, SemiRing<R>, MonoidP.GroupP<R> {

	/**
	 * The integers under multiplication form a
	 * <a href="https://en.wikipedia.org/wiki/Commutative_ring">Commutative Ring</a>.
	 */
	interface Integer<I extends Integer<I>> extends Ring<I>, Integers {}
}
