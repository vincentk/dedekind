package com.github.vincentk.dedekind.algebra.peano;

import com.github.vincentk.dedekind.algebra.Ring.Naturals;

sealed 
interface Peano<N, P extends Peano<N, P>>
extends Naturals
{
	final class Zero implements Peano<Naturals, Zero> {
		private Zero() {}
		
		@Override
		public long asLong() {
			return 0;
		}
	}

	final class Succ<N, P extends Peano<N, P>> implements Peano<N, Succ<N, P>> {
		
		private final long val;
		
		private Succ(P pred) {
			val = pred.asLong() + 1;
		}

		@Override
		public long asLong() {
			return val;
		}
		
		public static
		<N, P extends Peano<N, P>>
		Succ<N, P> succ(P pred) {
			return new Succ<N, P>(pred);
		}
		
	}
	
	// Proof by type-check:
	public static final Zero ZERO = new Zero();
	public static final Succ<Naturals, Zero> ONE = Succ.succ(Zero.ZERO);
	public static final Succ<Naturals, Succ<Naturals, Zero>> TWO = Succ.succ(ONE);
	public static final Succ<Naturals, Succ<Naturals, Succ<Naturals, Zero>>> THREE = Succ.succ(TWO);
}
