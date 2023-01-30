package com.github.vincentk.dedekind.linear.lazy;

import com.github.vincentk.dedekind.algebra.Ring;
import com.github.vincentk.dedekind.linear.ColumnVector;
import com.github.vincentk.dedekind.linear.Matrix;
import com.github.vincentk.dedekind.linear.RowVector;

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
public final class ComposedMatrix<
//Ring:
F extends Ring<F>,

//Implementation detail:
R1 extends RowVector<F, C, R1>,
//Range of the linear map:
C extends ColumnVector<F, R1, C>,

//Implementation detail:
R2 extends RowVector<F, D, R2>,
//Domain of linear map:
D extends ColumnVector<F, R2, D>,

//Implementation detail:
R3 extends RowVector<F, E, R3>,
//Domain of linear map:
E extends ColumnVector<F, R3, E>
>
implements Matrix<F, R1, C, R2, D, ComposedMatrix<F, R1, C, R2, D, R3, E>>
{
    private final Matrix<F, R3, E, R2, D, ?> m1;
    private final Matrix<F, R1, C, R3, E, ?> m2;

    public ComposedMatrix(
            Matrix<F, R3, E, R2, D, ?> m1,
            Matrix<F, R1, C, R3, E, ?> m2
            ) {
        this.m1 = m1;
        this.m2 = m2;
    }

    public Matrix<F, R3, E, R2, D, ?> fst() {
        return m1;
    }

    public Matrix<F, R1, C, R3, E, ?> snd() {
        return m2;
    }

    @Override
    public MatrixAddition<F, R1, C, R2, D> plus(Matrix<F, R1, C, R2, D, ?> that) {
        return new MatrixAddition<F, R1, C, R2, D>(this, that);
    }

    @Override
    public C apply(D vector) {

        final var v1 = m1.apply(vector);
        final var v2 = m2.apply(v1);

        return v2;
    }

    @Override
    public Matrix<F, R2, D, R1, C, ?> transpose() {
        // (A B)' = B' A'
        final Matrix<F, R2, D, R3, E, ?> m1t = m1.transpose();
        final Matrix<F, R3, E, R1, C, ?> m2t = m2.transpose();

        return new ComposedMatrix<>(m2t, m1t);
    }

    @Override
    public
    <R4 extends RowVector<F, E2, R4>, E2 extends ColumnVector<F, R4, E2>>
    Matrix<F, R1, C, R4, E2, ?>
    compose(Matrix<F, R2, D, R4, E2, ?> other) {
        return new ComposedMatrix<>(other, this);
    }
}
