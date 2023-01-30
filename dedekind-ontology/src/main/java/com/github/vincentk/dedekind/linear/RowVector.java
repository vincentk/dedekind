package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.Ring;

/**
 * A "row vector" a.k.a. covector a.k. linear functional
 * 
 * @param <F> field
 * @param <D>
 * @param <C>
 */
public interface RowVector<
// Field elements:
F extends Ring<F>,
// Domain:
D extends ColumnVector<F, ?, D>,
// Recursive self-type:
S extends RowVector<F, D, S>
>
extends
// Is an element of a vector space:
Vector<F, S>,
// Is a linear map from column vectors to the underlying field:
//LinearMap<D, F>,
//The transpose is a row vector:
Dual<D>
{
}
