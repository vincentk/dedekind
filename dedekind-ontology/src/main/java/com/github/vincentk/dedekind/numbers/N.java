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

    public int integer();

    @Override
    default Ne plus(N that) {
        return nat(integer() + that.integer());
    }

    @Override
    default Ne times(N that) {
        return nat(integer() * that.integer());
    }

    @Override
    default boolean equals(N that) {
        return integer() == that.integer();
    }

    @Override
    default N abs() {
        return this;
    }

    @Override
    default Ne distance(N other) {
        return nat(Math.abs(integer() - other.integer()));
    }

    record Ne (int integer) implements N {

        public Ne(int integer) {
            assert integer >= 0;
            this.integer = integer;
        }

        @Override
        public int compareTo(N o) {
            return Integer.compare(integer, o.integer());
        }
    }

    static Ne nat(int n) {
        return new Ne(n);
    }
}
