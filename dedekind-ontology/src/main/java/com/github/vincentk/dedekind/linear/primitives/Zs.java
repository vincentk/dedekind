package com.github.vincentk.dedekind.linear.primitives;

import com.github.vincentk.dedekind.algebra.Equality;
import com.github.vincentk.dedekind.algebra.Ring;

public final class Zs implements Ring<Zs>, Equality<Zs> {

	private final int val;

	private Zs(int val) {
		this.val = val;
	}

	@Override
	public Zs plus(Zs that) {
		return new Zs(this.val + that.val);
	}

	@Override
	public Zs times(Zs that) {
		return new Zs(this.val * that.val);
	}

	@Override
	public boolean equals(Zs that) {
		return this.val == that.val;
	}

	@Override
	public boolean equals(Object that) {
		if (that instanceof Zs) {
			return equals((Zs) that);
		}
		return false;
	}

	@Override
	public String toString() {
		return String.valueOf(val);
	}

	@Override
	public Zs negate() {
		return of(-val);
	}

	public static Zs of(int val) {
		return new Zs(val);
	}

	public static Zs ZERO = of(0), ONE = of(1), TWO = of(2), THREE = of(3);
}
