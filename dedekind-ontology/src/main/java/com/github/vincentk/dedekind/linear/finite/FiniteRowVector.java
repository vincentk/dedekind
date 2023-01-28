package com.github.vincentk.dedekind.linear.finite;

import com.github.vincentk.dedekind.algebra.Ring;
import com.github.vincentk.dedekind.algebra.peano.Cardinality;
import com.github.vincentk.dedekind.linear.ColumnVector;
import com.github.vincentk.dedekind.linear.RowVector;

public interface FiniteRowVector<
// Field elements:
F extends Ring<F>,
// Cardinality:
C extends Cardinality,
// Domain:
D extends ColumnVector<F, ?, D>,
// Recursive self-type:
S extends FiniteRowVector<F, C, D, S>
>
extends
FiniteVector<F, C, S>,
RowVector<F, D, S>
{

}
