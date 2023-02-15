package com.github.vincentk.dedekind.numbers;

import com.github.vincentk.dedekind.algebra.Field;

/**
 * The set of complex numbers.
 */
public interface C extends Number<C>, Field.Complex<C>, MetricSpace<C, R> {

    public R re();
    public R im();

    public static final C ZERO = of(0, 0), R1 = of(1, 0), I1 = of(0, 1), UNIT = of(1, 1);

    public static C of(double re, double im) {
        return of(R.of(re), R.of(im));
    }

    public static C of(R re, R im) {
        return new Impl(re, im);
    }

    final class Impl implements C {
        private final R re, im;

        private Impl(R re, R im) {
            this.re = re;
            this.im = im;
        }

        public R re() {
            return re;
        }

        public R im() {
            return im;
        }

        @Override
        public C plus(C that) {
            return of(re.p(that.re()), im.p(that.im()));
        }


        @Override
        public C negate() {
            return of(re.neg(), im.neg());
        }

        @Override
        public C inverse() {

            final var xs = conj(); 
            final R r2 = re.x(re);

            return of(xs.re().div(r2), xs.im().div(r2));
        }

        @Override
        public C minus(C that) {
            return of(re.minus(that.re()), this.im.minus(that.im()));
        }

        @Override
        public C times(C that) {

            final var r2 = re.x(that.re());
            final var i2 = im.x(that.im());

            final var ri = re.x(that.im());
            final var ir = im.x(that.re());

            return of(r2.minus(i2), ri.p(ir));
        }

        @Override
        public C conjugate() {
            return of(re, im.neg());
        }


        @Override
        public R distance(C other) {
            return minus(other).abs();
        }
        
        @Override
        public boolean equals(C that) {
            return this.re.equals(that.re()) && this.im.equals(that.im());
        }

        @Override
        public boolean equals(Object that) {
            if (that instanceof C) {
                return equals((C) that);
            }
            return false;
        }

        @Override
        public String toString() {
            return "(" + re + "," + im + ")";
        }

        @Override
        public R abs() {
            return times(conj()).re().sqrt();
        }
    }
}
