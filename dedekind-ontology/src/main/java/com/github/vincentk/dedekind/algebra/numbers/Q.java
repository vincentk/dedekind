package com.github.vincentk.dedekind.algebra.numbers;

import static com.github.vincentk.dedekind.algebra.numbers.Z.Z64.integer;

import java.util.Optional;

import com.github.vincentk.dedekind.algebra.numbers.Z.Z64;
import com.github.vincentk.dedekind.algebra.numbers.Z.Z64.Int64;
import com.github.vincentk.dedekind.algebra.structures.Field;
import com.github.vincentk.dedekind.geometry.MetricSpace;
import com.github.vincentk.dedekind.geometry.NumberLine;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Countable;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered;

/**
 * An implementation of rational numbers.
 */
public interface Q extends
Countable<Q.Rat, Cardinality.Countable, Q>,
NumberLine<Q.Rat, Cardinality.Countable, Q>,
Field.Rationals<Q.Rat, Q>
{
    public static final Q.Rat ZERO = rational(0, 1), UNIT = rational(1, 1);

    public static Rat rational(long en, long de) {
	return rational(integer(en), integer(de));
    }

    public static Rat rational(Int64 en, Int64 de) {
	return new Impl(en, de).simplify();
    }

    interface Rat
    extends
    Field.Fe<Rat>,
    TotallyOrdered.Oe<Rat>,
    MetricSpace.MeG<Rat, Rat>
    {
	public Int64 en();
	public Int64 de();

	@Override
	default Rat negate() {
	    return rational(en().neg(), de());
	}

	@Override
	default Optional<Rat> inverse() {
	    if (de().isIdentityP()) return Optional.empty();
	    else return Optional.of(rational(de(), en()));
	}

	@Override
	default int compareTo(Rat o) {

	    // Enumerators given same denominator (d1 * d2):
	    final var e1 = en().times(o.de());
	    final var e2 = o.en().times(this.de());

	    return e1.compareTo(e2);
	}

	@Override
	default Rat plus(Rat that) {

	    final var en1 = en().x(that.de()).十(that.en().x(de()));
	    final var de1 = de().x(that.de());

	    return rational(en1, de1);
	}

	@Override
	default Rat times(Rat that) {

	    final var en = en().times(that.en());
	    final var de = de().times(that.de());

	    return rational(en, de);
	}

	@Override
	default Rat abs() {

	    // Should be guaranteed by constructor:
	    assert de().compareTo(Z64.ZERO) > 0;

	    return en().compareTo(Z64.ZERO) >= 0 ? this : this.neg();
	}
    }

    record Impl (Int64 en, Int64 de) implements Rat {


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
	public Impl(Int64 en, Int64 de) {

	    assert !de.eq(Z64.ZERO);

	    if (de.intValue() >= 0) {
		this.en = en;
		this.de = de;                
	    } else {
		this.en = en.neg();
		this.de = de.neg();
	    }

	}

	public Rat simplify() {

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

	private static final int[] COMMON_PRIMES = new int[] {2, 3, 5, 7, 11, 13 };
    }
}
