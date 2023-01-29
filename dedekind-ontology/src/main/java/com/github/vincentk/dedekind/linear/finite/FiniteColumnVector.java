package com.github.vincentk.dedekind.linear.finite;

import com.github.vincentk.dedekind.algebra.Ring;
import com.github.vincentk.dedekind.algebra.peano.Cardinality;
import com.github.vincentk.dedekind.linear.ColumnVector;

public interface FiniteColumnVector<
// Field elements:
F extends Ring<F>,
// Cardinality:
C extends Cardinality,
// Domain:
D extends FiniteRowVector<F, C, S, D>,
// Recursive self-type:
S extends FiniteColumnVector<F, C, D, S>
>
extends
FiniteVector<F, C, S>,
ColumnVector<F, D, S>
{

}
