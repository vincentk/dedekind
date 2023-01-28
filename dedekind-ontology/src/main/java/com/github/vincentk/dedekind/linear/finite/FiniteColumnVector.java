package com.github.vincentk.dedekind.linear.finite;

import com.github.vincentk.dedekind.algebra.Ring;
import com.github.vincentk.dedekind.algebra.peano.Cardinality;
import com.github.vincentk.dedekind.linear.ColumnVector;
import com.github.vincentk.dedekind.linear.RowVector;

public interface FiniteColumnVector<
// Field elements:
F extends Ring<F>,
// Cardinality:
C extends Cardinality,
// Domain:
D extends RowVector<F, ?, D>,
// Recursive self-type:
S extends FiniteColumnVector<F, C, D, S>
>
extends
FiniteVector<F, C, S>,
ColumnVector<F, D, S>
{

}
