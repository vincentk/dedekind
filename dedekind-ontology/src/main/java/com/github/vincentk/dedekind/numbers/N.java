/**
 * 
 */
package com.github.vincentk.dedekind.numbers;

import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Set;

/**
 * The natural numbers.
 */
public interface N
extends
Number<N>,
Set.TotalOrder<Cardinality.Countable, N>,
SemiRing.Natural<N>,
MetricSpace<N, N>
{
    
    public int intValue();

    static final class Impl implements N {
        
        private final int n;
        
        private Impl(int n) {
            this.n = n;
        }
        
        @Override
        public Impl plus(N that) {
            return of(n + that.intValue());
        }

        @Override
        public Impl times(N that) {
            return of(n * that.intValue());
        }

        @Override
        public int intValue() {
            return n;
        }

        @Override
        public int compareTo(N o) {
            return Integer.compare(n, o.intValue());
        }

        @Override
        public boolean equals(N that) {
            return n == that.intValue();
        }

        @Override
        public N distance(N other) {
            return of(Math.abs(n - other.intValue()));
        }

        @Override
        public N abs() {
            return this;
        }
    }
    
    static Impl of(int n) {
        return new Impl(n);
    }
}
