package com.github.vincentk.dedekind.linear.finite;

import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.linear.ColumnVector;
import com.github.vincentk.dedekind.sets.Cardinality;

public interface FiniteColumnVector<
// Field elements:
F extends SemiRing<F>,
// Cardinality:
C extends Cardinality.Finite,
// Domain:
D extends FiniteRowVector<F, C, ?, D>,
// Recursive self-type:
S extends FiniteColumnVector<F, C, D, S>
>
extends
FiniteVector<F, C, D, S>,
ColumnVector<F, C, D, S>
{
}
