package com.github.vincentk.dedekind.algebra.numbers;

import java.util.Optional;

import com.github.vincentk.dedekind.algebra.structures.Field;
import com.github.vincentk.dedekind.geometry.MetricSpace;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered;

/**
 * The set of real numbers.
 */
public interface R
extends
NumberLine<R.R64, Cardinality.Uncountable, R>,
Field.Reals<R.R64, R>
{
    public static final R64 ZERO = real(0), ONE = real(1), TWO = real(2), THREE = real(3);

    public static R64 real(double val) {
	return new Re(val);
    }

    interface R64
    extends
    Field.Fe<R64>,
    TotallyOrdered.Oe<R64>,
    MetricSpace.MeG<R64, R64>
    {
	double doubleVal();

	@Override
	default R64 plus(R64 that) {
	    return real(doubleVal() + that.doubleVal());
	}

	@Override
	default R64 times(R64 that) {
	    return real(doubleVal() * that.doubleVal());
	}

	@Override
	default R64 negate() {
	    return real(-doubleVal());
	}

	@Override
	default Optional<R64> inverse() {
	    if (0 == doubleVal()) return Optional.empty();
	    return Optional.of(real(1.0 / doubleVal()));
	}

	@Override
	default boolean eq(R64 that) {

	    if (doubleVal() == that.doubleVal()) {
		// Exact numeric equality:
		return true;
	    }

	    final double ad = Math.abs(doubleVal() - that.doubleVal());
	    final double av = (doubleVal() + that.doubleVal()) / 2;

	    final double err = ad / av;
	    return err < 10E-10;
	}

	@Override
	default int compareTo(R64 o) {
	    return Double.compare(doubleVal(), o.doubleVal());
	}

	@Override
	default R64 abs() {
	    return doubleVal() >= 0 ? this : this.neg();
	}

	default R64 sqrt() {
	    return real(Math.sqrt(doubleVal()));
	}
    }

    record Re (double doubleVal) implements R64 {

	@Override
	public boolean equals(Object that) {
	    if (!(that instanceof R64)) return false;
	    return eq((R64) that);
	}
    }
}
