package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.Ring;

/**
 * A "column vector".
 * 
 * @param <F> field
 * @param <D>
 * @param <C>
 */
public interface ColumnVector<
// Field elements:
F extends Ring<F>,
// Domain:
D extends RowVector<F, S, D>,
// Recursive self-type:
S extends ColumnVector<F, D, S>
>
extends
// Is an element of a vector space:
Vector<F, S>,
// Is a linear map from column vectors to the underlying field:
//LinearMap<F, D, F>,
// The transpose is a row vector:
Dual<D>
{
}
