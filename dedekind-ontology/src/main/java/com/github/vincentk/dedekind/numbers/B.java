/**
 * 
 */
package com.github.vincentk.dedekind.numbers;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.SemiRings.Booleans;
import com.github.vincentk.dedekind.sets.Set;

/**
 * Boolean values. Roughly speaking 0 <=> false, 1 <=> true.
 */
public interface B extends Number<B>, Set.TotalOrder<Cardinality.Finite, B>, Booleans {

    public boolean booleanValue();
    
    /**
     * a || b ~ a + b
     * 
     * a || 0 = a => zero is the monoid unit (~ addition).
     * 
     * @param that
     * @return boolean sum (or)
     */
    default B and(B that) {
        return this.p(that);
    }
    
    /**
     * 
     * a && b ~ a * b
     * 
     * a && 1 = a => 1 is the monoid unit (~ multiplication).
     * 
     * @param that
     * @return boolean product (and)
     */
    default B or(B that) {
        return this.x(that);
    }

    record Be (boolean booleanValue) implements B {
        
        @Override
        public Be plus(B that) {
            return of(booleanValue || that.booleanValue());
        }

        @Override
        public Be times(B that) {
            return of(booleanValue && that.booleanValue());
        }

        @Override
        public int compareTo(B o) {
            return Boolean.compare(booleanValue, o.booleanValue());
        }

        @Override
        public boolean equals(B that) {
            return booleanValue == that.booleanValue();
        }
    }
    
    static Be of(boolean n) {
        return new Be(n);
    }
}
