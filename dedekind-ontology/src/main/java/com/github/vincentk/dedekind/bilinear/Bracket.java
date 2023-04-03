/**
 * 
 */
package com.github.vincentk.dedekind.bilinear;

import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.algebra.binary.SemiModule;

/**
 * @see https://en.wikipedia.org/wiki/Bra%E2%80%93ket_notation
 * 
 * @param <F> field
 * @param <D> dual
 * @param <S> self
 */
public interface Bracket<
F extends SemiRing<F>,
D extends Dual<S, D>,
S extends Bracket<F, D, S>
>
extends
// |x>' = <x| etc.:
Dual<D, S>,
// |x> + |x> = 2 |x> etc.:
SemiModule<F, S>
{
    /**
     * A "row vector" ~ covector ~ linear functional ~ bra ~ <x|
     * 
     * @see https://en.wikipedia.org/wiki/Inner_product_space
     * @see https://en.wikipedia.org/wiki/Linear_form
     * 
     * @param <F> field
     * @param <K> ket
     * @param <S> self
     */
    public interface Bra<
    F extends SemiRing<F>,
    K extends Ket<F, S, K>,
    S extends Bra<F, K, S>
    >
    extends
    Bracket<F, K, S> {

        /**
         * Inner product declaration.
         * 
         * @param |y>
         * @return <x|y>
         */
        F dot(K ket);
    }

    /**
     * A "column vector" ~ ket ~ |x> .
     *
     * @param <F> field
     * @param <B> bra
     * @param <S> self
     */
    public interface Ket<
    F extends SemiRing<F>,
    B extends Bra<F, S, B>,
    S extends Ket<F, B, S>
    >
    extends
    Bracket<F, B, S> {

        /**
         * Outer product a.k.a. tensor product.
         * 
         * Column vector x row vector -> matrix.
         * 
         * @param <x|
         * @return |y><x|
         */
        <
        K1 extends Ket<F, B1, K1>,
        B1 extends Bra<F, K1, B1>
        >
        OuterProduct<F, S, B, K1, B1>
        outer(B1 bra);
    }
}
