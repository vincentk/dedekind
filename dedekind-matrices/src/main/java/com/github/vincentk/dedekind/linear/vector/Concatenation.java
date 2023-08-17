/**
 * 
 */
package com.github.vincentk.dedekind.linear.vector;

import com.github.vincentk.dedekind.algebra.binary.Module;
import com.github.vincentk.dedekind.algebra.unary.Ring;
import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * Vector concatenation arises frequently in practical applications.
 * I am not aware of a corresponding concept in abstract algebra, perhaps
 * because it is possible to express via matrix multiplication + addition.
 * 
 * E.g. [a] + [b] = [a, b]
 * 
 * FIXME: presently overly restrictive PoC.
 * TODO: generalize to vectors with infinite cardinality.
 */
public interface Concatenation<
F extends Ring<F>,

C1 extends Cardinality.Finite,
K1 extends Module<F, C1, K1>,


C2 extends Cardinality,
B2 extends Module<F, C2, B2>,

S extends Concatenation<F, C1, K1, C2, B2, S>
>
extends
Module<F, C1, S>
{
    K1 fst();
    B2 snd();
    
    S clone(K1 fst, B2 snd);

    @Override
    default S mult(F scalar) {
        return clone(fst().mult(scalar), snd().mult(scalar));
    }

    @Override
    default S plus(S vector) {
        return clone(fst().plus(vector.fst()), snd().plus(vector.snd()));
    }
    

    @Override
    default S zero() {
        return clone(fst().zero(), snd().zero());
    }

    @Override
    default S negate() {
        return clone(fst().negate(), snd().negate());
    }

    public record Impl<
    F extends Ring<F>,

    C1 extends Cardinality.Finite,
    K1 extends Module<F, C1, K1>,


    C2 extends Cardinality,
    B2 extends Module<F, C2, B2>
    >(K1 fst, B2 snd)
    implements Concatenation<F, C1, K1, C2, B2, Impl<F, C1, K1, C2, B2>>{

        @Override
        public Impl<F, C1, K1, C2, B2> clone(K1 fst, B2 snd) {
            return new Impl<>(fst, snd);
        }
    }
}
