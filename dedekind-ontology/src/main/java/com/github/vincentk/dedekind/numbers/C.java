package com.github.vincentk.dedekind.numbers;

import com.github.vincentk.dedekind.relation.binary.homogeneous.Field;

/**
 * The set of complex numbers.
 */
public interface C extends Number<C>, Field.Complex<C>, MetricSpace<C, R> {

    public R re();
    public R im();

    public static final C ZERO = complex(0, 0), R1 = complex(1, 0), I1 = complex(0, 1), UNIT = complex(1, 1);

    public static C complex(double re, double im) {
        return of(R.real(re), R.real(im));
    }

    public static C of(R re, R im) {
        return new Ce(re, im);
    }

    record Ce (R re, R im) implements C {

        @Override
        public C plus(C that) {
            return of(re.十(that.re()), im.十(that.im()));
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
        public C times(C that) {

            final var r2 = re.x(that.re());
            final var i2 = im.x(that.im());

            final var ri = re.x(that.im());
            final var ir = im.x(that.re());

            return of(r2.minus(i2), ri.十(ir));
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
        public R abs() {
            return times(conj()).re().sqrt();
        }
    }
}
