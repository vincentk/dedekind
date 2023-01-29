package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.Ring;

/**
 * Very roughly speaking, definition of a matrix as a vector of vectors.
 */
public interface Matrix<
// Ring:
F extends Ring<F>,
// Domain of linear map:
D extends Vector<F, D>,
// Self-reference:
M extends Matrix<F, D, M>>
extends
Vector<F, M>
{

    /**
     * A column-major matrix.
     * 
     * @see https://en.wikipedia.org/wiki/Row-_and_column-major_order
     * 
     * @param <F>
     * @param <T>
     * @param <D>
     * @param <R>
     * @param <S>
     */
    public static interface ColumnMajor<
    // Ring:
    F extends Ring<F>,
    // Type of transposed row implementation:
    T extends ColumnVector<F, D, T>,
    // Domain of linear map:
    D extends RowVector<F, T, D>,
    // Type of dual matrix:
    R extends RowMajor<F, T, D, S, R>,
    // Self-reference:
    S extends ColumnMajor<F, T, D, R, S>>
    extends
    Matrix<F, D, S>,
    RowVector<F, R, S>
    {

    }
    
    /**
     * A row-major matrix.
     * 
     * @see https://en.wikipedia.org/wiki/Row-_and_column-major_order
     * 
     * @param <F>
     * @param <T>
     * @param <D>
     * @param <R>
     * @param <S>
     */
    public static interface RowMajor<
    // Ring:
    F extends Ring<F>,
    // Type of transposed row implementation:
    T extends ColumnVector<F, D, T>,
    // Domain of linear map:
    D extends RowVector<F, T, D>,
    // Type of dual matrix:
    C extends ColumnMajor<F, T, D, S, C>,
    // Self-reference:
    S extends RowMajor<F, T, D, C, S>>
    extends
    Matrix<F, D, S>,
    ColumnVector<F, C, S>
    {

    }
}
