/**
 * 
 */
package com.github.vincentk.dedekind.numbers;

import com.github.vincentk.dedekind.algebra.Ring;

/**
 * The integer numbers.
 */
public interface Z extends Ring<Z> {
    
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
    }
    
    static Z of(int n) {
        return new Impl(n);
    }
}
