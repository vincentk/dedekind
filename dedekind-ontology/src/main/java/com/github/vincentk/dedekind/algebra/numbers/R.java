package com.github.vincentk.dedekind.algebra.numbers;

import com.github.vincentk.dedekind.relation.binary.homogeneous.Field;
import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * The set of real numbers.
 */
public interface R extends NumberLine<Cardinality.Uncountable, R>, Field.Reals<R> {

    public static final R ZERO = real(0), ONE = real(1), TWO = real(2), THREE = real(3);

    public static R real(double val) {
        return new Re(val);
    }

    double doubleVal();

    R sqrt();

    record Re (double doubleVal) implements R {

        @Override
        public R plus(R that) {
            return real(this.doubleVal + that.doubleVal());
        }

        @Override
        public R times(R that) {
            return real(this.doubleVal * that.doubleVal());
        }

        @Override
        public R divide(R that) {
            return real(this.doubleVal / that.doubleVal());
        }

        @Override
        public R negate() {
            return real(-doubleVal);
        }

        @Override
        public R inverse() {
            return real(1.0 / doubleVal);
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
            return real(Math.sqrt(doubleVal));
        }
    }
}
