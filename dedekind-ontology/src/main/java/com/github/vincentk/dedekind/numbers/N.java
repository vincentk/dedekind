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

    record Ne (int intValue) implements N {

        @Override
        public Ne plus(N that) {
            return of(intValue + that.intValue());
        }

        @Override
        public Ne times(N that) {
            return of(intValue * that.intValue());
        }

        @Override
        public int compareTo(N o) {
            return Integer.compare(intValue, o.intValue());
        }

        @Override
        public boolean equals(N that) {
            return intValue == that.intValue();
        }

        @Override
        public N distance(N other) {
            return of(Math.abs(intValue - other.intValue()));
        }

        @Override
        public N abs() {
            return this;
        }
    }

    static Ne of(int n) {
        return new Ne(n);
    }
}
