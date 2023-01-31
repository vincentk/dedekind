package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.Ring;

/**
 * @see https://en.wikipedia.org/wiki/Inner_product_space
 * @see https://en.wikipedia.org/wiki/Bra%E2%80%93ket_notation
 */
public interface InnerProductSpace {
    
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
    // Is an element of a vector space:
    Vector<F, S>,
    // The transpose is a row vector:
    Dual<K>
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
     * @param <F> field
     * @param <B>
     * @param <C>
     * 
     * @see https://en.wikipedia.org/wiki/Linear_form
     */
    public interface Ket<
    F extends Ring<F>,
    B extends Bra<F, S, B>,
    S extends Ket<F, B, S>
    >
    extends
    // Is an element of a vector space:
    Vector<F, S>,
    //The transpose is a column vector:
    Dual<B>
    {
    }
}
