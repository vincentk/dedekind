/**
 * 
 */
package com.github.vincentk.dedekind.linear.matrix;

import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.bilinear.InnerProductSpace.Bra;
import com.github.vincentk.dedekind.bilinear.InnerProductSpace.Ket;

/**
 * A square matrix domain and range are essentially the same.
 */
public interface Square<
// Ring:
F extends SemiRing<F>,

//Implementation detail:
R2 extends Bra<F, D, R2>,
// Domain of linear map:
D extends Ket<F, R2, D>,

// Type short-hand for change of basis (see below).
S extends Square<F, R2, D, S, ?>,

// Self-reference:
M extends Square<F, R2, D, S, M>
>
extends
// A square matrix is a matrix where domain and range are the same:
Matrix<F, R2, D, R2, D, M>
// Multiplication of two square matrices produces a new square matrix of the same dimension:
// Monoid.M<S>
{
    // Transposing a square matrix gives another square matrix:
    @Override
    Square<F, R2, D, ?, ?> transpose();
    
    // Matrix multiplication with a square matrix.
    // Essentially a change of basis.
    // Can presumably be refined.
    // @Override
    // S times(S that);
}