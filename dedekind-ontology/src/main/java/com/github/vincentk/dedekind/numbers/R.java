package com.github.vincentk.dedekind.numbers;

import com.github.vincentk.dedekind.algebra.Field;
import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * The set of real numbers.
 */
public interface R extends NumberLine<Cardinality.Uncountable, R>, Field.Reals<R> {

    public static final R ZERO = of(0), ONE = of(1), TWO = of(2), THREE = of(3);

    public static R of(double val) {
        return new Re(val);
    }

    double doubleVal();

    R sqrt();

    record Re (double doubleVal) implements R {

        @Override
        public R plus(R that) {
            return of(this.doubleVal + that.doubleVal());
        }

        @Override
        public R minus(R that) {
            return of(this.doubleVal - that.doubleVal());
        }

        @Override
        public R times(R that) {
            return of(this.doubleVal * that.doubleVal());
        }

        @Override
        public R divide(R that) {
            return of(this.doubleVal / that.doubleVal());
        }

        @Override
        public R negate() {
            return of(-doubleVal);
        }

        @Override
        public R inverse() {
            return of(1.0 / doubleVal);
        }

        @Override
        public boolean equals(R that) {
            
            if (doubleVal == that.doubleVal()) {
                // Exact numeric equality:
                return true;
            }
            
            final double ad = Math.abs(doubleVal - that.doubleVal());
            final double av = (doubleVal + that.doubleVal()) / 2;
            
            final double err = ad / av;
            return err < 10E-10;
        }

        @Override
        public boolean equals(Object that) {
            if (that instanceof R) {
                return equals((R) that);
            }
            return false;
        }

        @Override
        public int compareTo(R o) {
            return Double.compare(doubleVal, o.doubleVal());
        }

        @Override
        public R abs() {
            return doubleVal >= 0 ? this : this.neg();
        }

        @Override
        public R sqrt() {
            return of(Math.sqrt(doubleVal));
        }
    }
}
