package com.github.vincentk.dedekind.bilinear;

import com.github.vincentk.dedekind.algebra.structures.Bracket.Bra;
import com.github.vincentk.dedekind.algebra.structures.Bracket.Ket;
import com.github.vincentk.dedekind.algebra.structures.SemiRing;
import com.github.vincentk.dedekind.linear.matrix.Matrix;

/**
 * Outer product / tensor product implementation.
 * 
 * @see https://en.wikipedia.org/wiki/Outer_product
 */
public record OuterProduct<
R extends SemiRing.SmrE<R>,

K1 extends Ket<R, B1, K1>,
B1 extends Bra<R, K1, B1>,

K2 extends Ket<R, B2, K2>,
B2 extends Bra<R, K2, B2>
>
(K1 ket, B2 bra)
implements
Matrix<R, K1, K2, OuterProduct<R, K1, B1, K2, B2>>
{

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

    /**
     * a |y><x| = |ay><x|
     */
    @Override
    public OuterProduct<R, K1, B1, K2, B2> mult(R scalar) {
        return new OuterProduct<>(ket.mult(scalar), bra);
    }

    /**
     * (|y><x|)' = |x><y|
     */
    @Override
    public OuterProduct<R, K2, B2, K1, B1> transpose() {
        return new OuterProduct<>(bra.transpose(), ket.transpose());
    }

    @Override
    public boolean eq(Matrix<R, K1, K2, ?> that) {
	return this == that;
    }
}
