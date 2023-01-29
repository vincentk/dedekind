package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.Ring;

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
R1 extends RowVector<F, C, R1>,
//Range of the linear map:
C extends ColumnVector<F, R1, C>,

//Implementation detail:
R2 extends RowVector<F, D, R2>,
//Domain of linear map:
D extends ColumnVector<F, R2, D>
>
implements Matrix<F, R1, C, R2, D, MatrixAddition<F, R1, C, R2, D>>
{
    private final Matrix<F, R1, C, R2, D, ?> m1, m2;
    
    MatrixAddition(
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
    public Matrix<F, R2, D, R1, C, ?> transpose() {
        // (A + B)' = A' + B'
        return new MatrixAddition<>(m1.transpose(), m2.transpose());
    }

    @Override
    public
    <R4 extends RowVector<F, E2, R4>, E2 extends ColumnVector<F, R4, E2>>
    Matrix<F, R1, C, R4, E2, ?> compose(
            Matrix<F, R2, D, R4, E2, ?> other) {
        return new ComposedMatrix<>(other, this);
    }
}
