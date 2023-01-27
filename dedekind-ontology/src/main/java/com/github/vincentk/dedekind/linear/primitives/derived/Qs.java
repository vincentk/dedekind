package com.github.vincentk.dedekind.linear.primitives.derived;

import com.github.vincentk.dedekind.algebra.Equality;
import com.github.vincentk.dedekind.algebra.Field;
import com.github.vincentk.dedekind.linear.primitives.Zs;

/**
 * An implementation of rational numbers.
 */
public final class Qs implements Field.Rationals<Qs>, Equality<Qs>{

	public static final Qs ZERO = of(0, 1), UNIT = of(1, 1);

	private final Zs en, de;

	private Qs(Zs en, Zs de) {
		this.en = en;

		assert !de.equals(Zs.ZERO);
		this.de = de;
	}

	public static Qs of(int en, int de) {
		return of(Zs.of(en), Zs.of(de));
	}

	public static Qs of(Zs en, Zs de) {
		return new Qs(en, de).simplify();
	}

	public Qs simplify() {

		final int eni = en.intValue();

		if (eni == 1) {
			return this;
		}

		final int dei = de.intValue();

		if (dei == 1) {
			return this;
		}

		if (eni == 0) {
			// 0 / x = 0 = 0 / 1
			return of(0, 1);
		}

		// Factor some common primes:
		for (final int pi : COMMON_PRIMES) {

			// If both denominator and the enumerator are divisible
			// by the same prime number, divide both by that number and recurse:
			if (eni % pi  == 0 && dei % pi == 0) {
				return of(eni / pi, dei / pi);
			}
		}

		// Simplify exactly if the denominator is
		// an integer multiple of the enumerator or
		// vice versa.
		final int mod1 = eni % dei;
		if (mod1 == 0) {
			// Enumerator is integer multiple of denominator.
			// The result is an integer number:
			return of(eni / dei, 1);
		}

		// else:
		final int mod2 = dei % eni;
		if (mod2 == 0) {
			// Denominator is integer multiple of enumerator.
			return of(1, dei / eni);
		}

		// To be more sophisticated, factorization might
		// be needed, which we will not do here:
		return this;
	}

	@Override
	public Qs plus(Qs that) {

		final var en1 = en.x(that.de).p(that.en.times(de));
		final var de1 = de.times(that.de);

		return of(en1, de1);
	}


	@Override
	public Qs negate() {
		return of(en.neg(), de);
	}

	@Override
	public Qs inverse() {
		return of(de, en);
	}

	@Override
	public Qs minus(Qs that) {
		return of(en.minus(that.en), this.de.minus(that.de));
	}

	@Override
	public Qs times(Qs that) {

		final var en = this.en.times(that.en);
		final var de = this.de.times(that.de);

		return of(en, de);
	}

	@Override
	public boolean equals(Qs that) {
		return this.en.equals(that.en) && this.de.equals(that.de);
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
		return "(" + en + "," + de + ")";
	}

	private static final int[] COMMON_PRIMES = new int[] {2, 3, 5, 7, 11, 13 };
}
