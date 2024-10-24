/**
 * 
 */
package com.github.vincentk.dedekind.algebra.numbers;

import static java.lang.Long.compare;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.Ring;

/**
 * The integer numbers.
 */
public interface Z extends NumberLine<Cardinality.Countable, Z>, Ring.Integer<Z> {

    public long intValue();

    record Impl (long intValue) implements Z {

        @Override
        public Z plus(Z that) {
            return integer(intValue + that.intValue());
        }

        @Override
        public Z times(Z that) {
            return integer(intValue * that.intValue());
        }

        @Override
        public Z negate() {
            return integer(-intValue);
        }

        @Override
        public int compareTo(Z o) {
            return compare(intValue, o.intValue());
        }

        @Override
        public boolean eq(Z that) {
            return intValue == that.intValue();
        }

        @Override
        public Z abs() {
            return intValue >= 0 ? this : this.neg();
        }
    }

    static Z integer(long n) {
        return new Impl(n);
    }

    public static final Z ZERO = integer(0), ONE = integer(1), TWO = integer(2), THREE = integer(3);
}
