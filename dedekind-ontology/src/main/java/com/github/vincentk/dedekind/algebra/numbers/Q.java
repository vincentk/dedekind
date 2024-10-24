package com.github.vincentk.dedekind.algebra.numbers;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.Equality;
import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.Field;

/**
 * An implementation of rational numbers.
 */
public interface Q extends
NumberLine<Cardinality.Countable, Q>,
Field.Rationals<Q>,
Equality<Q> {

    public static final Q ZERO = rational(0, 1), UNIT = rational(1, 1);

    public static Q rational(long en, long de) {
	return rational(Z.integer(en), Z.integer(de));
    }

    public static Q rational(Z en, Z de) {
	return new Impl(en, de).simplify();
    }

    public Z en();
    public Z de();

    @Override
    default Q negate() {
	return rational(en().neg(), de());
    }

    @Override
    default Q inverse() {
	return rational(de(), en());
    }

    @Override
    default int compareTo(Q o) {

	// Enumerators given same denominator (d1 * d2):
	final var e1 = en().times(o.de());
	final var e2 = o.en().times(this.de());

	return e1.compareTo(e2);
    }

    @Override
    default Q plus(Q that) {

	final var en1 = en().x(that.de()).十(that.en().x(de()));
	final var de1 = de().x(that.de());

	return rational(en1, de1);
    }

    @Override
    default Q times(Q that) {

	final var en = en().times(that.en());
	final var de = de().times(that.de());

	return rational(en, de);
    }

    @Override
    default Q abs() {

	// Should be guaranteed by constructor:
	assert de().compareTo(Z.ZERO) > 0;

	return en().compareTo(Z.ZERO) >= 0 ? this : this.neg();
    }

    record Impl (Z en, Z de) implements Q {


	/**
	 * It is possible to ensure that the denominator is always positive:
	 * 
	 * e.g. given a / b
	 * 
	 * if b < 0 then => - a / - b
	 * 
	 * @param en
	 * @param de
	 */
	public Impl(Z en, Z de) {

	    assert !de.eq(Z.ZERO);

	    if (de.intValue() >= 0) {
		this.en = en;
		this.de = de;                
	    } else {
		this.en = en.neg();
		this.de = de.neg();
	    }

	}

	public Q simplify() {

	    final long eni = en.intValue();

	    if (Math.abs(eni) == 1) {
		return this;
	    }

	    final long dei = de.intValue();

	    if (dei == 1) {
		return this;
	    }

	    if (eni == 0) {
		// 0 / x = 0 = 0 / 1
		return rational(0, 1);
	    }

	    // Factor some common primes:
	    for (final int pi : COMMON_PRIMES) {

		// If both denominator and the enumerator are divisible
		// by the same prime number, divide both by that number and recurse:
		if (eni % pi  == 0 && dei % pi == 0) {
		    return rational(eni / pi, dei / pi);
		}
	    }

	    // Simplify exactly if the denominator is
	    // an integer multiple of the enumerator or
	    // vice versa.
	    final long mod1 = eni % dei;
	    if (mod1 == 0) {
		// Enumerator is integer multiple of denominator.
		// The result is an integer number:
		return rational(eni / dei, 1);
	    }

	    // else:
	    final long mod2 = dei % eni;
	    if (mod2 == 0) {
		// Denominator is integer multiple of enumerator.                
		return rational(1, dei / eni);
	    }

	    // To be more sophisticated, factorization might
	    // be needed, which we will not do here:
	    return this;
	}

	@Override
	public boolean eq(Q that) {
	    return this.en.eq(that.en()) && this.de.eq(that.de());
	}

	@Override
	public boolean equals(Object that) {
	    if (that instanceof Q) {
		return eq((Q) that);
	    }
	    return false;
	}

	private static final int[] COMMON_PRIMES = new int[] {2, 3, 5, 7, 11, 13 };
    }
}
