package com.github.vincentk.dedekind.linear.primitives;

import com.github.vincentk.dedekind.algebra.Equality;
import com.github.vincentk.dedekind.algebra.Ring;

public final class Ints {

	static final class Z implements Ring<Z>, Equality<Z> {

		private final int val;

		Z(int val) {
			this.val = val;
		}

		@Override
		public Z plus(Z that) {
			return new Z(this.val + that.val);
		}

		@Override
		public Z times(Z that) {
			return new Z(this.val * that.val);
		}

		@Override
		public boolean equals(Z that) {
			return this.val == that.val;
		}
		
		@Override
		public boolean equals(Object that) {
			if (that instanceof Z) {
				return equals((Z) that);
			}
			return false;
		}
		
		@Override
		public String toString() {
			return String.valueOf(val);
		}
	}
	
	public static Z of(int val) {
		return new Z(val);
	}

	public static Z ZERO = of(0), ONE = of(1), TWO = of(2), THREE = of(3);
}
