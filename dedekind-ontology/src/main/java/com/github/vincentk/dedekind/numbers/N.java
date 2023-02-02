/**
 * 
 */
package com.github.vincentk.dedekind.numbers;

import com.github.vincentk.dedekind.algebra.SemiRing;

/**
 * The natural numbers.
 */
public interface N extends SemiRing<N> {
    
    public int intValue();

    static final class Impl implements N {
        
        private final int n;
        
        private Impl(int n) {
            this.n = n;
        }
        
        @Override
        public N plus(N that) {
            return of(n + that.intValue());
        }

        @Override
        public N times(N that) {
            return of(n * that.intValue());
        }

        @Override
        public int intValue() {
            return n;
        }
    }
    
    static N of(int n) {
        return new Impl(n);
    }
}
