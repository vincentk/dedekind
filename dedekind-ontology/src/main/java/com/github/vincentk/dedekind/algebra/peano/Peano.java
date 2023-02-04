package com.github.vincentk.dedekind.algebra.peano;

import com.github.vincentk.dedekind.algebra.SemiRing.Natural;
import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * Implementation of Peano Numbers / Axioms.
 * 
 * https://en.wikipedia.org/wiki/Peano_axioms
 * 
 * @param <P>
 */
public sealed 
interface Peano<N extends Peano<N>>
extends
Natural<Peano<?>>,
Cardinality.Finite
{
    default Peano<N> plus(Zero that) {
        return this;
    }

    final class Zero implements Peano<Zero> {

        private Zero() {}

        @Override
        public Peano<?> plus(Peano<?> that) {
            return that;
        }

        @Override
        public Peano<?> times(Peano<?> that) {
            return this;
        }

        @Override
        public long cardinality() {
            return 0;
        }
    }

    final class Succ<N extends Peano<N>> implements Peano<Succ<N>> {

        private final N pred;

        private Succ(N pred) {
            this.pred = pred;
        }

        @Override
        public Peano<?> plus(Peano<?> that) {

            // a + 0 = a
            if (that instanceof Zero) {
                return this;
            }

            // a + S(b) = S(a + b) = S(a) + b
            return succ(this).plus(pred(that));
        }

        @Override
        public Peano<?> times(Peano<?> that) {

            // a * 0 = 0
            if (that instanceof Zero) {
                return that;
            }

            // a * S(b) = a + (a * b)
            return this.plus(this.times(pred(that)));
        }

        private static Peano<?> pred(Peano<?> succ) {
            return ((Succ<?>)succ).pred;
        }

        @Override
        public long cardinality() {
            return 1 + pred.cardinality();
        }

    }

    private static
    <P extends Peano<P>>
    Succ<P> succ(P pred) {
        return new Succ<>(pred);
    }

    // Proof by type-check:
    public static final Zero ZERO = new Zero();
    public static final Succ<Zero> ONE = succ(ZERO);
    public static final Succ<Succ<Zero>> TWO = succ(ONE);
    public static final Succ<Succ<Succ<Zero>>> THREE = succ(TWO);
}
