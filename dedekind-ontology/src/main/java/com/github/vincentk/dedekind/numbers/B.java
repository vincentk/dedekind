/**
 * 
 */
package com.github.vincentk.dedekind.numbers;

import com.github.vincentk.dedekind.algebra.unary.Ring;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.SemiRings.Booleans;
import com.github.vincentk.dedekind.sets.Set;

/**
 * Boolean values. Roughly speaking 0 <=> false, 1 <=> true.
 */
public interface B extends Number<B>, Ring<B>, Set.TotalOrder<Cardinality.Finite, B>, Booleans {

    public boolean bool();

    /**
     * a && b ~ a * b
     * 
     * a && 1 = a => 1 is the monoid unit (~ multiplication).
     * 
     * @param that
     * @return boolean product (and)
     */
    default B and(B that) {
        return x(that);
    }

    /**
     * a || b ~ a + b
     * 
     * a || 0 = a => zero is the monoid unit (~ addition).
     * 
     * @param that
     * @return boolean sum (or)
     */
    default B or(B that) {
        return p(that);
    }

    @Override
    default Be plus(B that) {
        return bool(bool() || that.bool());
    }

    @Override
    default Be negate() {
        return bool(!bool());
    }

    @Override
    default Be times(B that) {
        return bool(bool() && that.bool());
    }

    @Override
    default int compareTo(B o) {
        return Boolean.compare(bool(), o.bool());
    }

    @Override
    default B zero() {
        return FALSE;
    }

    default N nat() {
        return bool() ? N.ONE : N.ZERO;
    }

    static Be bool(boolean n) {
        return n ? TRUE : FALSE;
    }

    record Be (boolean bool) implements B {

        @Override
        public boolean equals(B that) {
            return bool() == that.bool();
        }
    }

    public static final Be TRUE = new Be(true), FALSE = new Be(false);
}
