package com.github.vincentk.dedekind.algebra;

import com.github.vincentk.dedekind.sets.Monoids;

/**
 * Monoid with respect to "multiplication".
 * 
 * @param <M>
 */
public interface MonoidM<M extends MonoidM<M>> extends Multiplication<M, M>, Monoids {

    @Override
    M times(M that);

    public interface GroupM<G extends GroupM<G>> extends MonoidM<G> {

        /**
         * @return - this
         */
        G negate();

        default G neg() {
            return negate();
        }
    }
}