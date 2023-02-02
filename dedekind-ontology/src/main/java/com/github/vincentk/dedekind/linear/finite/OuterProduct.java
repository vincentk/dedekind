package com.github.vincentk.dedekind.linear.finite;

import com.github.vincentk.dedekind.algebra.Ring;
import com.github.vincentk.dedekind.linear.LinearMap;
import com.github.vincentk.dedekind.linear.OuterProductSpace.Bra;
import com.github.vincentk.dedekind.linear.OuterProductSpace.Ket;

/**
 * Outer product / tensor product implementation.
 * 
 * @see https://en.wikipedia.org/wiki/Outer_product
 *
 * @param <R>
 * @param <K1>
 * @param <B1>
 * @param <K2>
 * @param <B2>
 */
public final class OuterProduct<
R extends Ring<R>,

K1 extends Ket<R, B1, K1>,
B1 extends Bra<R, K1, B1>,

K2 extends Ket<R, B2, K2>,
B2 extends Bra<R, K2, B2>
>
implements
LinearMap<R, K2, K1>{
    
    private final K1 ket;
    private final B2 bra;
    
    public OuterProduct(K1 ket, B2 bra) {
        this.ket = ket;
        this.bra = bra;
    }

    /**
     * (x y') z = x * (y' z) = x * a = ax .
     */
    @Override
    public K1 apply(K2 vec) {
        
        // Associative law:
        final R sc = bra.dot(vec);
        
        // Scalar multiplication commutes:
        final K1 col = ket.mult(sc);
        
        return col;
    }
}