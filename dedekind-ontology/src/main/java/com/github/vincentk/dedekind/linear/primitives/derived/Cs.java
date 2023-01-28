package com.github.vincentk.dedekind.linear.primitives.derived;

import com.github.vincentk.dedekind.algebra.Equality;
import com.github.vincentk.dedekind.algebra.Field;
import com.github.vincentk.dedekind.linear.primitives.Rs;

/**
 * The set of complex numbers.
 */
public final class Cs implements Field.Complex<Cs>, Equality<Cs>{

    public static final Cs ZERO = of(0, 0), R1 = of(1, 0), I1 = of(0, 1), UNIT = of(1, 1);

    private final Rs re, im;

    private Cs(Rs re, Rs im) {
        this.re = re;
        this.im = im;
    }

    public static Cs of(double re, double im) {
        return of(Rs.of(re), Rs.of(im));
    }

    public static Cs of(Rs re, Rs im) {
        return new Cs(re, im);
    }

    @Override
    public Cs plus(Cs that) {
        return of(re.p(that.re), im.p(that.im));
    }


    @Override
    public Cs negate() {
        return of(re.neg(), im.neg());
    }

    @Override
    public Cs inverse() {

        final var xs = conj(); 
        final Rs r2 = re.x(re);

        return of(xs.re.div(r2), xs.im.div(r2));
    }

    @Override
    public Cs minus(Cs that) {
        return of(re.minus(that.re), this.im.minus(that.im));
    }

    @Override
    public Cs times(Cs that) {

        final var r2 = re.x(that.re);
        final var i2 = im.x(that.im);

        final var ri = re.x(that.im);
        final var ir = im.x(that.re);

        return of(r2.minus(i2), ri.p(ir));
    }

    @Override
    public Cs conjugate() {
        return of(re, im.neg());
    }

    @Override
    public boolean equals(Cs that) {
        return this.re.equals(that.re) && this.im.equals(that.im);
    }

    @Override
    public boolean equals(Object that) {
        if (that instanceof Cs) {
            return equals((Cs) that);
        }
        return false;
    }

    @Override
    public String toString() {
        return "(" + re + "," + im + ")";
    }
}
