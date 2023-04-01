package com.github.vincentk.dedekind.bilinear;

import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.bilinear.Bracket.Bra;
import com.github.vincentk.dedekind.bilinear.Bracket.Ket;
import com.github.vincentk.dedekind.linear.LinearMap;

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
public record OuterProduct<
R extends SemiRing<R>,

K1 extends Ket<R, B1, K1>,
B1 extends Bra<R, K1, B1>,

K2 extends Ket<R, B2, K2>,
B2 extends Bra<R, K2, B2>
>
(K1 ket, B2 bra)
implements
LinearMap<R, K2, K1> {

    /**
     * (x y') z = x * (y' z) = x * a = ax .
     */
    @Override
    public K1 apply(K2 ket2) {
        
        // Associative law, giving rise to a scalar:
        final R sc = bra.dot(ket2);
        
        // Scalar multiplication commutes:
        final K1 col = ket.mult(sc);
        
        return col;
    }
}
