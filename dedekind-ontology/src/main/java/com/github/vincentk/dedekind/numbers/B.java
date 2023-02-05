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
public interface B extends Number<B>, Set.Po<Cardinality.Finite, B>, Booleans {

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

    static final class Impl implements B {
        
        private final boolean n;
        
        private Impl(boolean n) {
            this.n = n;
        }
        
        @Override
        public Impl plus(B that) {
            return of(n || that.booleanValue());
        }

        @Override
        public Impl times(B that) {
            return of(n && that.booleanValue());
        }

        @Override
        public boolean booleanValue() {
            return n;
        }

        @Override
        public int compareTo(B o) {
            return Boolean.compare(n, o.booleanValue());
        }

        @Override
        public boolean equals(B that) {
            return n == that.booleanValue();
        }
    }
    
    static Impl of(boolean n) {
        return new Impl(n);
    }
}
