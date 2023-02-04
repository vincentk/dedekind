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
    }
    
    static Z of(int n) {
        return new Impl(n);
    }
}
