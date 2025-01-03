package com.github.vincentk.dedekind.algebra.numbers;

import java.util.Optional;

import com.github.vincentk.dedekind.algebra.structures.Field;

/**
 * The set of dual numbers.
 * 
 * @see https://en.wikipedia.org/wiki/Dual_number
 */
public record Ds (R.R64 re, R.R64 ep) implements Field.Fe<Ds> {

    public static final Ds ZERO = of(0, 0), R1 = of(1, 0), I1 = of(0, 1), UNIT = of(1, 1);

    public static Ds of(double re) {
	return of(re, 0);
    }

    public static Ds of(double re, double im) {
	return of(R.real(re), R.real(im));
    }

    public static Ds of(R.R64 re, R.R64 im) {
	return new Ds(re, im);
    }

    @Override
    public Ds plus(Ds that) {
	return of(re.十(that.re), ep.十(that.ep));
    }


    @Override
    public Ds negate() {
	return of(re.neg(), ep.neg());
    }

    @Override
    public Optional<Ds> inverse() {

	return re().inverse().map(rei -> { 
	    final var c2i = rei.abs2();
	    final var ei = ep().neg().times(c2i);
	    return of(c2i, ei);
	});
    }

    @Override
    public Ds times(Ds that) {

	final var ri = re.x(that.ep);
	final var ir = ep.x(that.re);

	return of(re.x(that.re), ri.十(ir));
    }

    @Override
    public boolean eq(Ds that) {
	return this.re.eq(that.re) && this.ep.eq(that.ep);
    }
}
