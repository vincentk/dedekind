package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.Ring;

/**
 * <p>
 * Note that in a certain sense, matrixes define 
 * <a href="https://en.wikipedia.org/wiki/Vector_space#Definition">vector spaces</a>.
 * In particular they satisfy the axioms of scalar multiplication and vector addition (matrix addition in this case), which
 * we choose to make this explicit by extending {@link Vector}.
 * </p>
 * <p>
 * Optional type tags, deliberately left unconstrained:
 * </p>
 * @param <F> the field, often a {@link Field}.
 * @param <D> the codomain.
 * @param <C> the domain.
 */
public interface Matrix<
// Ring:
F extends Ring<F>,
// Domain of linear map:
D extends ColumnVector<F, ?, D>,
// Codomain of linear map:
C extends ColumnVector<F, ?, C>,
// Self-reference:
M extends Matrix<F, D, C, M>>
extends 
LinearMap<D, C>,
Vector<F, M>,
Dual<Matrix<F, C, D, ?>>
{

    @Override
    M mult(F scalar);

    @Override
    M plus(M matrix);

    /**(
     * @param <M2> type of matrix returned.
     * @return the transposed matrix.
     *
     * @see https://en.wikipedia.org/wiki/Matrix_(mathematics)#Addition,_scalar_multiplication_and_transposition
     */
    Matrix<F, C, D, ?> transpose();

    /**
     * Multiplication with a column vector in the domain D.
     *
     * @param vector a vector in the domain D.
     * @return a column vector in the co-domain C.
     */
    default C vectorMult(D vector) {
    	return apply(vector);
    }

    /**
     * <a href="https://en.wikipedia.org/wiki/Matrix_multiplication">Matrix multiplication.</>
     * E.g. multiplying a 2x3 matrix with a 3x1 matrix gives a 2x1 matrix.
     *
     * @param matrix
     * @param <D2>
     * @param <M2>
     * @param <M3>
     * @return
     */
    <D2 extends ColumnVector<F, ?, D2>, M2 extends Matrix<F, C, D2, M2>, M3 extends Matrix<F, D2, D, M3>>
    M3
    mult(M2 matrix);
}
