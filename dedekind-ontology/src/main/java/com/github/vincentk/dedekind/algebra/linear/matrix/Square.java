/**
 * 
 */
package com.github.vincentk.dedekind.algebra.linear.matrix;

import com.github.vincentk.dedekind.algebra.binary.linear.Array;
import com.github.vincentk.dedekind.algebra.binary.linear.Matrix;
import com.github.vincentk.dedekind.algebra.unary.Ring;
import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * A square matrix domain and range are essentially the same.
 */
public interface Square<
// Ring:
F extends Ring<F>,

C extends Cardinality.Finite,
D extends Array<F, C, D>,

// Type short-hand for change of basis (see below).
S extends Square<F, C, D, S, ?>,

// Self-reference:
M extends Square<F, C, D, S, M>
>
extends
// A square matrix is a matrix where domain and range are the same:
Matrix<F, C, D, C, D, M>
// Multiplication of two square matrices produces a new square matrix of the same dimension:
// Monoid.M<S>
{
    // Transposing a square matrix gives another square matrix:
    @Override
    Square<F, C, D, ?, ?> transpose();
}