/**
 * 
 */
package com.github.vincentk.dedekind.numbers;

import static java.lang.Integer.compare;

import com.github.vincentk.dedekind.algebra.Ring;
import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * The integer numbers.
 */
public interface Z extends NumberLine<Cardinality.Countable, Z>, Ring.Integer<Z> {

    public int intValue();

    record Impl (int intValue) implements Z {

        @Override
        public Z plus(Z that) {
            return of(intValue + that.intValue());
        }

        @Override
        public Z times(Z that) {
            return of(intValue * that.intValue());
        }

        @Override
        public Z negate() {
            return of(-intValue);
        }

        @Override
        public int compareTo(Z o) {
            return compare(intValue, o.intValue());
        }

        @Override
        public boolean equals(Z that) {
            return intValue == that.intValue();
        }

        @Override
        public Z abs() {
            return intValue >= 0 ? this : this.neg();
        }
    }

    static Z of(int n) {
        return new Impl(n);
    }

    public static final Z ZERO = of(0), ONE = of(1), TWO = of(2), THREE = of(3);
}
