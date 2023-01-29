package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.MonoidM;
import com.github.vincentk.dedekind.algebra.MonoidP;
import com.github.vincentk.dedekind.algebra.Ring;

/**
 * @see https://en.wikipedia.org/wiki/Matrix_(mathematics)
 */
public interface Matrix<
// Ring:
F extends Ring<F>,

// Implementation detail:
R1 extends RowVector<F, C, R1>,
// Range of the linear map:
C extends ColumnVector<F, R1, C>,

//Implementation detail:
R2 extends RowVector<F, D, R2>,
// Domain of linear map:
D extends ColumnVector<F, R2, D>,

// Self-reference:
M extends Matrix<F, R1, C, R2, D, M>
>
extends
// Addition is supported for matrices of the same type:
MonoidP<M>,
// Any matrix is a linear map from a vector in the domain to the co-domain:
LinearMap<D, C>,
// A transpose is defined:
Dual<Matrix<F, R2, D, R1, C, ?>>
{
    @Override
    M plus(M that);

    @Override
    C apply(D vector);
    
    @Override
    Matrix<F, R2, D, R1, C, ?> transpose();

    /**
     * Matrix multiplication is defined if the range of the argument matches
     * the domain of this matrix.
     * 
     * The result is another matrix where the domain matches the range of this
     * matrix.
     * 
     * @param other
     * @return
     */
    Matrix<F, ?, ?, R1, C, ?> matmult(Matrix<F, R2, D, ?, ?, ?> other);

    /**
     * A square matrix domain and range are essentially the same.
     */
    public interface Square<
    // Ring:
    F extends Ring<F>,

    //Implementation detail:
    R2 extends RowVector<F, D, R2>,
    // Domain of linear map:
    D extends ColumnVector<F, R2, D>,

    // Self-reference:
    M extends Square<F, R2, D, M>
    >
    extends
    // A square matrix is a matrix where domain and range are the same:
    Matrix<F, R2, D, R2, D, M>,
    // Multiplication of two square matrices produces a new square matrix of the same dimension:
    MonoidM<M>
    {
        // Transposing a square matrix gives another square matrix:
        @Override
        Square<F, R2, D, ?> transpose();
    }
}
