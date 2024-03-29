package com.github.vincentk.dedekind.numbers;

import com.github.vincentk.dedekind.algebra.Equality;
import com.github.vincentk.dedekind.algebra.unary.Field;

/**
 * The set of dual numbers.
 * 
 * @see https://en.wikipedia.org/wiki/Dual_number
 */
public record Ds (R re, R ep) implements Field.Duals<Ds>, Equality<Ds>{

    public static final Ds ZERO = of(0, 0), R1 = of(1, 0), I1 = of(0, 1), UNIT = of(1, 1);

    public static Ds of(double re) {
        return of(re, 0);
    }

    public static Ds of(double re, double im) {
        return of(R.real(re), R.real(im));
    }

    public static Ds of(R re, R im) {
        return new Ds(re, im);
    }

    @Override
    public Ds plus(Ds that) {
        return of(re.p(that.re), ep.p(that.ep));
    }


    @Override
    public Ds negate() {
        return of(re.neg(), ep.neg());
    }

    @Override
    public Ds inverse() {

        final R ci = re.inverse();

        final R c2i = ci.times(ci);
        final R ei = ep.neg().times(c2i);

        return of(ci, ei);
    }

    @Override
    public Ds minus(Ds that) {
        return of(re.minus(that.re), this.ep.minus(that.ep));
    }

    @Override
    public Ds times(Ds that) {

        final var ri = re.x(that.ep);
        final var ir = ep.x(that.re);

        return of(re.x(that.re), ri.p(ir));
    }

    @Override
    public boolean equals(Ds that) {
        return this.re.equals(that.re) && this.ep.equals(that.ep);
    }
}
