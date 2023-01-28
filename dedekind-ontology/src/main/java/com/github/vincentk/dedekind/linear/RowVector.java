package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.Ring;
import com.github.vincentk.dedekind.algebra.peano.Peano;

/**
 * A "row vector" a.k.a. covector a.k. linear functional
 */
public interface RowVector<
// Field elements:
F extends Ring<F>,
// Domain:
D extends Vector<F, D>, 
// Codomain is the vectors of length 1:
C extends Vector<F, C> & Cardinality<Peano.Succ<Peano.Zero>>>
extends
// Is an element of a vector space:
Vector<F, RowVector<F, D, C>>,
// Is a matrix with exactly one column:
Matrix<F, D, C, RowVector<F, D, C>>
{
	// Re-stating to force a type-check:
	@Override
	C apply(D domain);
}
