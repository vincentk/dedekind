package com.github.vincentk.dedekind.numbers;

import com.github.vincentk.dedekind.algebra.Field;

/**
 * The set of real numbers.
 */
public interface R extends Number<R>, Field.Reals<R> {

    public static final R ZERO = of(0), ONE = of(1), TWO = of(2), THREE = of(3);

    public static R of(double val) {
        return new Impl(val);
    }

    double doubleVal();

    final class Impl implements R {

        private final double val;

        private Impl(double val) {
            this.val = val;
        }

        @Override
        public double doubleVal() {
            return val;
        }

        @Override
        public R plus(R that) {
            return of(this.val + that.doubleVal());
        }

        @Override
        public R minus(R that) {
            return of(this.val - that.doubleVal());
        }

        @Override
        public R times(R that) {
            return of(this.val * that.doubleVal());
        }

        @Override
        public R divide(R that) {
            return of(this.val / that.doubleVal());
        }

        @Override
        public R negate() {
            return of(-val);
        }

        @Override
        public R inverse() {
            return of(1.0 / val);
        }

        @Override
        public boolean equals(R that) {
            return this.val == that.doubleVal();
        }

        @Override
        public boolean equals(Object that) {
            if (that instanceof R) {
                return equals((R) that);
            }
            return false;
        }

        @Override
        public String toString() {
            return String.valueOf(val);
        }
    }
}
