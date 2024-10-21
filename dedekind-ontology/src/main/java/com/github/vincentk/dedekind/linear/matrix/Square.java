/**
 * 
 */
package com.github.vincentk.dedekind.linear.matrix;

import com.github.vincentk.dedekind.relation.binary.Bracket.Ket;
import com.github.vincentk.dedekind.relation.binary.homogeneous.SemiRing;

/**
 * A square matrix domain and range are essentially the same.
 */
public interface Square<
// Ring:
F extends SemiRing<F>,

// Domain of linear map:
D extends Ket<F, ?, D>,

// Type short-hand for change of basis (see below).
S extends Square<F, D, S, ?>,

// Self-reference:
M extends Square<F, D, S, M>
>
extends
// A square matrix is a matrix where domain and range are the same:
Matrix<F, D, D, M>
// Multiplication of two square matrices produces a new square matrix of the same dimension:
// Monoid.M<S>
{
    // Transposing a square matrix gives another square matrix:
    @Override
    Square<F, D, ?, ?> transpose();
    
    // Matrix multiplication with a square matrix.
    // Essentially a change of basis.
    // Can presumably be refined.
    // @Override
    // S times(S that);
}