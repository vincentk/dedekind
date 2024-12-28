package com.github.vincentk.dedekind.algebra.structures;

import com.github.vincentk.dedekind.bilinear.OuterProduct;

/**
 * @see https://en.wikipedia.org/wiki/Bra%E2%80%93ket_notation
 * 
 * @param <F> fieldO
 * @param <D> dual
 * @param <S> self
 */
public interface Bracket<
E extends SemiRing.SmrE<E>,
D extends Dual<S, D>,
S extends Bracket<E, D, S>
>
extends
// |x>' = <x| etc.:
Dual<D, S>,
// |x> + |x> = 2 |x> etc.:
SemiModule.SmE<E, S>
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
    E extends SemiRing.SmrE<E>,
    K extends Ket<E, S, K>,
    S extends Bra<E, K, S>
    >
    extends
    Bracket<E, K, S> {

        /**
         * Inner product declaration.
         * 
         * @param |y>
         * @return <x|y>
         */
        E dot(K ket);
    }

    /**
     * A "column vector" ~ ket ~ |x> .
     *
     * @param <F> field
     * @param <B> bra
     * @param <S> self
     */
    public interface Ket<
    E extends SemiRing.SmrE<E>,
    B extends Bra<E, S, B>,
    S extends Ket<E, B, S>
    >
    extends
    Bracket<E, B, S> {

        /**
         * Outer product a.k.a. tensor product.
         * 
         * Column vector x row vector -> matrix.
         * 
         * @param <x|
         * @return |y><x|
         */
        <
        K1 extends Ket<E, B1, K1>,
        B1 extends Bra<E, K1, B1>
        >
        OuterProduct<E, S, B, K1, B1>
        outer(B1 bra);
    }
}
