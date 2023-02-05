package com.github.vincentk.dedekind.numbers;

import com.github.vincentk.dedekind.algebra.Equality;
import com.github.vincentk.dedekind.algebra.Field;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Set;

/**
 * An implementation of rational numbers.
 */
public interface Q extends
Number<Q>,
Field.Rationals<Q>,
Set.Po<Cardinality.Countable, Q>,
Equality<Q> {

    public static final Q ZERO = of(0, 1), UNIT = of(1, 1);

    public static Q of(int en, int de) {
        return of(Z.of(en), Z.of(de));
    }

    public static Q of(Z en, Z de) {
        return new Impl(en, de).simplify();
    }
    
    public Z en();
    public Z de();
    
    /**
     * It is possible to ensure that the denominator is always positive:
     * 
     * e.g. given a / b
     * 
     * if b < 0 then => - a / - b
     * 
     * @return the same value but 
     */
    default public Q positiveDenominator() {
        if(de().intValue() >= 0) {
            return this;
        }
        return of(en().neg(), de().neg());
    }
    
    final class Impl implements Q {
        
        private final Z en, de;

        private Impl(Z en, Z de) {
            this.en = en;

            assert !de.equals(Z.ZERO);
            this.de = de;
        }

        @Override
        public Z en() {
            return en;
        }

        @Override
        public Z de() {
            return de;
        }

        public Q simplify() {

            final int eni = en.intValue();

            if (Math.abs(eni) == 1) {
                return this;
            }

            final int dei = de.intValue();

            if (dei == 1) {
                return this;
            }

            if (eni == 0) {
                // 0 / x = 0 = 0 / 1
                return of(0, 1);
            }

            // Factor some common primes:
            for (final int pi : COMMON_PRIMES) {

                // If both denominator and the enumerator are divisible
                // by the same prime number, divide both by that number and recurse:
                if (eni % pi  == 0 && dei % pi == 0) {
                    return of(eni / pi, dei / pi);
                }
            }

            // Simplify exactly if the denominator is
            // an integer multiple of the enumerator or
            // vice versa.
            final int mod1 = eni % dei;
            if (mod1 == 0) {
                // Enumerator is integer multiple of denominator.
                // The result is an integer number:
                return of(eni / dei, 1);
            }

            // else:
            final int mod2 = dei % eni;
            if (mod2 == 0) {
                // Denominator is integer multiple of enumerator.                
                return of(1, dei / eni);
            }

            // To be more sophisticated, factorization might
            // be needed, which we will not do here:
            return this;
        }

        @Override
        public Q plus(Q that) {

            final var en1 = en.x(that.de()).p(that.en().times(de));
            final var de1 = de.times(that.de());

            return of(en1, de1);
        }


        @Override
        public Q negate() {
            return of(en.neg(), de);
        }

        @Override
        public Q inverse() {
            return of(de, en);
        }

        @Override
        public Q minus(Q that) {
            return of(en.minus(that.en()), this.de.minus(that.de()));
        }

        @Override
        public Q times(Q that) {

            final var en = this.en.times(that.en());
            final var de = this.de.times(that.de());

            return of(en, de);
        }

        @Override
        public boolean equals(Q that) {
            return this.en.equals(that.en()) && this.de.equals(that.de());
        }

        @Override
        public boolean equals(Object that) {
            if (that instanceof Q) {
                return equals((Q) that);
            }
            return false;
        }

        @Override
        public String toString() {
            return "(" + en + "," + de + ")";
        }

        @Override
        public int compareTo(Q o) {
            
            final var q1 = this.positiveDenominator();
            final var q2 = o.positiveDenominator();
            
            // Enumerators given same denominator (d1 * d2):
            final var e1 = q1.en().times(q2.de());
            final var e2 = q2.en().times(q1.de());
            
            return e1.compareTo(e2);
        }
        
        private static final int[] COMMON_PRIMES = new int[] {2, 3, 5, 7, 11, 13 };

    }
}
