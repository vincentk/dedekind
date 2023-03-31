package com.github.vincentk.dedekind.linear.lazy;

import com.github.vincentk.dedekind.algebra.Ring;
import com.github.vincentk.dedekind.linear.InnerProductSpace.Bra;
import com.github.vincentk.dedekind.linear.InnerProductSpace.Ket;
import com.github.vincentk.dedekind.linear.matrix.Matrix;

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
public final class MatrixAddition<
//Ring:
F extends Ring<F>,

//Implementation detail:
R1 extends Bra<F, C, R1>,
//Range of the linear map:
C extends Ket<F, R1, C>,

//Implementation detail:
R2 extends Bra<F, D, R2>,
//Domain of linear map:
D extends Ket<F, R2, D>
>
implements Matrix<F, R1, C, R2, D, MatrixAddition<F, R1, C, R2, D>>
{
    private final Matrix<F, R1, C, R2, D, ?> m1, m2;

    public MatrixAddition(
            Matrix<F, R1, C, R2, D, ?> m1,
            Matrix<F, R1, C, R2, D, ?> m2
            ) {
        this.m1 = m1;
        this.m2 = m2;
    }

    public Matrix<F, R1, C, R2, D, ?> fst() {
        return m1;
    }

    public Matrix<F, R1, C, R2, D, ?> snd() {
        return m2;
    }

    @Override
    public MatrixAddition<F, R1, C, R2, D> plus(Matrix<F, R1, C, R2, D, ?> that) {
        return new MatrixAddition<>(this, that);
    }

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
    public
    <R4 extends Bra<F, E2, R4>, E2 extends Ket<F, R4, E2>>
    Matrix<F, R1, C, R4, E2, ?> compose(
            Matrix<F, R2, D, R4, E2, ?> other) {
        return new MatrixMultiplication<>(other, this);
    }

    @Override
    public MatrixAddition<F, R1, C, R2, D> mult(F scalar) {
        // Distributive law:
        // (A + B) * a = (A * a + B * a)
        return new MatrixAddition<>(m1.mult(scalar), m2.mult(scalar));
    }
}
