package com.github.vincentk.dedekind.linear.matrix;

import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.bilinear.Bracket.Bra;
import com.github.vincentk.dedekind.bilinear.Bracket.Ket;

/**
 * A lazy / symbolic implementation of matrix multiplication.
 * 
 * @param <F>
 * @param <R1>
 * @param <C>
 * @param <R2>
 * @param <D>
 * @param <R3>
 * @param <E>
 */
public record MatrixMultiplication<
//Ring:
F extends SemiRing<F>,

//Implementation detail:
R1 extends Bra<F, C, R1>,
//Range of the linear map:
C extends Ket<F, R1, C>,

//Implementation detail:
R2 extends Bra<F, D, R2>,
//Domain of linear map:
D extends Ket<F, R2, D>,

//Implementation detail:
R3 extends Bra<F, E, R3>,
//Domain of linear map:
E extends Ket<F, R3, E>
>
(
        Matrix<F, R3, E, R2, D, ?> m1,
        Matrix<F, R1, C, R3, E, ?> m2
)
implements Matrix<F, R1, C, R2, D, MatrixMultiplication<F, R1, C, R2, D, R3, E>>
{
    @Override
    public C apply(D vector) {

        final var v1 = m1.apply(vector);
        final var v2 = m2.apply(v1);

        return v2;
    }

    @Override
    public MatrixMultiplication<F, R2, D, R1, C, R3, E> transpose() {
        // (A B)' = B' A'
        final Matrix<F, R2, D, R3, E, ?> m1t = m1.transpose();
        final Matrix<F, R3, E, R1, C, ?> m2t = m2.transpose();

        return new MatrixMultiplication<>(m2t, m1t);
    }

    @Override
    public MatrixMultiplication<F, R1, C, R2, D, R3, E> mult(F scalar) {
        // Associative law. Multiplication with a scalar commutes.
        // We can choose any of
        // (A x B) a  = (A * a) x B = A x (b * a)
        return new MatrixMultiplication<>(m1.mult(scalar), m2);
    }

    @Override
    public MatrixMultiplication<F, R1, C, R2, D, R3, E> negate() {
        return new MatrixMultiplication<>(m1.negate(), m2);
    }
}
