package com.github.vincentk.dedekind.linear.primitives.derived;

import com.github.vincentk.dedekind.algebra.Equality;
import com.github.vincentk.dedekind.algebra.Field;
import com.github.vincentk.dedekind.linear.primitives.Zs;

/**
 * The set of complex numbers.
 */
public final class Qs implements Field.Rationals<Qs>, Equality<Qs>{
	
	public static final Qs ZERO = of(0, 1), UNIT = of(1, 1);

	private final Zs re, im;

	private Qs(Zs re, Zs im) {
		this.re = re;
		
		assert !im.equals(Zs.ZERO);
		this.im = im;
	}
	
	public static Qs of(int re, int im) {
		return of(Zs.of(re), Zs.of(im));
	}
	
	public static Qs of(Zs re, Zs im) {
		return new Qs(re, im);
	}

	@Override
	public Qs plus(Qs that) {
		return of(re.p(that.re), im.p(that.im));
	}
	

	@Override
	public Qs negate() {
		return of(re.neg(), im);
	}

	@Override
	public Qs inverse() {
		return of(im, re);
	}

	@Override
	public Qs minus(Qs that) {
		return of(re.minus(that.re), this.im.minus(that.im));
	}

	@Override
	public Qs times(Qs that) {
		
		final var en = this.re.times(that.re);
		final var de = this.im.times(that.im);
		
		return of(en, de);
	}

	@Override
	public boolean equals(Qs that) {
		return this.re.equals(that.re) && this.im.equals(that.im);
	}
	
	@Override
	public boolean equals(Object that) {
		if (that instanceof Qs) {
			return equals((Qs) that);
		}
		return false;
	}
	
	@Override
	public String toString() {
		return "(" + re + "," + im + ")";
	}
}
