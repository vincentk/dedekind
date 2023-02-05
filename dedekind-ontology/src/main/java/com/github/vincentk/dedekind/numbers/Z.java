/**
 * 
 */
package com.github.vincentk.dedekind.numbers;

import com.github.vincentk.dedekind.algebra.Ring;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Rings.Integers;
import com.github.vincentk.dedekind.sets.Set;

/**
 * The integer numbers.
 */
public interface Z extends Ring<Z>, Number<Z>, Set.Po<Cardinality.Countable, Z>, Integers {

    public int intValue();

    static final class Impl implements Z {

        private final int n;

        private Impl(int n) {
            this.n = n;
        }

        @Override
        public int intValue() {
            return n;
        }

        @Override
        public Z plus(Z that) {
            return of(n + that.intValue());
        }

        @Override
        public Z times(Z that) {
            return of(n * that.intValue());
        }

        @Override
        public Z negate() {
            return of(-n);
        }

        @Override
        public int compareTo(Z o) {
            return Integer.compare(n, o.intValue());
        }

        @Override
        public boolean equals(Z that) {
            return n == that.intValue();
        }
        
        @Override
        public boolean equals(Object that) {
            
            if (that instanceof Z) {
                return equals((Z) that);
            }
            return false;
        }
        
        @Override
        public String toString() {
            return "Z@" + intValue();
        }
    }

    static Z of(int n) {
        return new Impl(n);
    }

    public static final Z ZERO = of(0), ONE = of(1), TWO = of(2), THREE = of(3);
}
