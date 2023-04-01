package com.github.vincentk.dedekind.linear.matrix;

import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.bilinear.Bracket.Bra;
import com.github.vincentk.dedekind.bilinear.Bracket.Ket;

/**
 * A lazy / symbolic implementation of matrix addition.
 * 
 * @param <F>
 * @param <R1>
 * @param <C>
 * @param <R2>
 * @param <D>
 * @param <R3>
 * @param <E>
 */
public record MatrixAddition<
//Ring:
F extends SemiRing<F>,

//Implementation detail:
R1 extends Bra<F, C, R1>,
//Range of the linear map:
C extends Ket<F, R1, C>,

//Implementation detail:
R2 extends Bra<F, D, R2>,
//Domain of linear map:
D extends Ket<F, R2, D>
>
(
        Matrix<F, R1, C, R2, D, ?> m1,
        Matrix<F, R1, C, R2, D, ?> m2
)
implements Matrix<F, R1, C, R2, D, MatrixAddition<F, R1, C, R2, D>>
{
    @Override
    public C apply(D vector) {

        final var v1 = m1.apply(vector);
        final var v2 = m2.apply(vector);

        return v1.plus(v2);
    }

    @Override
    public MatrixAddition<F, R2, D, R1, C> transpose() {
        // (A + B)' = A' + B'
        return new MatrixAddition<>(m1.transpose(), m2.transpose());
    }

    @Override
    public MatrixAddition<F, R1, C, R2, D> mult(F scalar) {
        // Distributive law:
        // (A + B) * a = (A * a + B * a)
        return new MatrixAddition<>(m1.mult(scalar), m2.mult(scalar));
    }
}
