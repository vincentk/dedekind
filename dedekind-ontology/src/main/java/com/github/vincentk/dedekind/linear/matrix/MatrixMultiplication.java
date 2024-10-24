package com.github.vincentk.dedekind.linear.matrix;

import com.github.vincentk.dedekind.sets.binary.relation.Bracket.Ket;
import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.SemiRing;

/**
 * A lazy / symbolic implementation of matrix multiplication.
 * 
 * @param <F>
 * @param <C>
 * @param <D>
 * @param <E>
 */
public record MatrixMultiplication<
F extends SemiRing<F>,
C extends Ket<F, ?, C>,
D extends Ket<F, ?, D>,
E extends Ket<F, ?, E>
>
(
        Matrix<F, E, D, ?> m1,
        Matrix<F, C, E, ?> m2
)
implements Matrix<F, C, D, MatrixMultiplication<F, C, D, E>>
{
    @Override
    public C apply(D vector) {

        final var v1 = m1.apply(vector);
        final var v2 = m2.apply(v1);

        return v2;
    }

    @Override
    public MatrixMultiplication<F, D, C, E> transpose() {
        // (A B)' = B' A'
        final Matrix<F, D, E, ?> m1t = m1.transpose();
        final Matrix<F, E, C, ?> m2t = m2.transpose();

        return new MatrixMultiplication<>(m2t, m1t);
    }

    @Override
    public MatrixMultiplication<F, C, D, E> mult(F scalar) {
        // Associative law. Multiplication with a scalar commutes.
        // We can choose any of
        // (A x B) a  = (A * a) x B = A x (b * a)
        return new MatrixMultiplication<>(m1.mult(scalar), m2);
    }

    @Override
    public boolean eq(Matrix<F, C, D, ?> that) {
	return this == that;
    }
}
