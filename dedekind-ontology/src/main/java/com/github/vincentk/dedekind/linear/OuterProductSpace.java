package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.Ring;

/**
 * An inner product space that also defines an outer product |x><y|
 * in addition to the inner product <x|y> .
 * 
 * @see https://en.wikipedia.org/wiki/Outer_product
 * @see https://en.wikipedia.org/wiki/Inner_product_space
 * @see https://en.wikipedia.org/wiki/Bra%E2%80%93ket_notation
 */
public interface OuterProductSpace extends InnerProductSpace {

    /**
     * A "row vector" ~ covector ~ linear functional ~ bra ~ <x|
     * 
     * @param <F> field
     * @param <K>
     * @param <C>
     */
    public interface Bra<
    F extends Ring<F>,
    K extends Ket<F, S, K>,
    S extends Bra<F, K, S>
    >
    extends
    InnerProductSpace.Bra<F, K, S>
    {
        /**
         * Inner product declaration.
         * 
         * @param ket
         * @return <x | y>
         */
        F dot(K ket);
    }

    /**
     * A "column vector" ~ ket ~ |x> .
     * 
     * @param <R> field
     * @param <B>
     * @param <C>
     * 
     * @see https://en.wikipedia.org/wiki/Linear_form
     */
    interface Ket<
    R extends Ring<R>,
    B extends Bra<R, K, B>,
    K extends Ket<R, B, K>
    >
    extends
    InnerProductSpace.Ket<R, B, K>
    {
        /**
         * Outer product a.k.a. tensor product.
         * 
         * Column vector x row vector -> matrix.
         * 
         * @param column
         * @return
         */
        <
        K1 extends Ket<R, B1, K1>,
        B1 extends Bra<R, K1, B1>
        >
        LinearMap<R, K1, K> outer(B1 bra);
    }
}
