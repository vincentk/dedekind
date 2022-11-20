package com.github.vincentk.dedekind.algebra.peano;

import com.github.vincentk.dedekind.algebra.Ring.Natural;

/**
 * Implementation of Peano Numbers.
 * 
 * https://en.wikipedia.org/wiki/Peano_axioms
 * 
 * @param <P>
 */
sealed 
interface Peano<P extends Peano<P>>
extends Natural<Peano<?>>
{
	long longVal();
	
	default Peano<?> plus(Zero that) {
		return this;
	}

	final class Zero implements Peano<Zero> {
		private Zero() {}

		@Override
		public Peano<?> plus(Peano<?> that) {
			return that;
		}

		@Override
		public long longVal() {
			return 0;
		}
	}

	final class Succ<P extends Peano<P>> implements Peano<Succ<P>> {

		private final P pred;

		private Succ(P pred) {
			this.pred = pred;
		}

		@Override
		public Peano<?> plus(Peano<?> that) {

			if (that instanceof Zero) {
				return this;
			}

			return succ(this).plus(((Succ<?>)that).pred);
		}

		@Override
		public long longVal() {
			return 1 + pred.longVal();
		}
	}

	private static
	<P extends Peano<P>>
	Succ<P> succ(P pred) {
		return new Succ<>(pred);
	}

	// Proof by type-check:
	public static final Zero ZERO = new Zero();
	public static final Succ<Zero> ONE = succ(Zero.ZERO);	
	public static final Succ<Succ<Zero>> TWO = succ(ONE);
	public static final Succ<Succ<Succ<Zero>>> THREE = succ(TWO);
}
