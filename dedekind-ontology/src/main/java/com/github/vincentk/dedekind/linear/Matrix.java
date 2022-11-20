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
 * @param <D> the codomain, often a {@link Vector}.
 * @param <C> the domain, often a {@link Vector}.
 */
public interface Matrix<F extends Ring<F>, D, C, M extends Matrix<F, D, C, M>> extends Vector<F, M> {

    @Override
    M mult(F scalar);

    @Override
    M plus(M vector);

    /**(
     * @param <M2> type of matrix returned.
     * @return the transposed matrix.
     *
     * @see https://en.wikipedia.org/wiki/Matrix_(mathematics)#Addition,_scalar_multiplication_and_transposition
     */
    <M2 extends Matrix<F, C, D, M2>>
    M2 transpose();

    /**
     * Multiplication with a column vector in the domain D.
     *
     * @param vector a vector in the domain D.
     * @return a column vector in the co-domain C.
     */
    C vectorMult(D vector);

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
    <D2, M2 extends Matrix<F, C, D2, M2>, M3 extends Matrix<F, D2, D, M3>>
    M3
    mult(M2 matrix);
}
