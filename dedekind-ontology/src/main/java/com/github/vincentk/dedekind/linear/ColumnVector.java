package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.Ring;

/**
 * A "column vector".
 */
public interface ColumnVector<
//Field elements:
F extends Ring<F>,
// Recursive self-type:
S extends ColumnVector<F, S>
>
extends
Vector<F, S>
{
	// tbd.
}
