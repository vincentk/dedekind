package com.github.vincentk.dedekind.linear.finite;

import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.linear.RowVector;
import com.github.vincentk.dedekind.sets.Cardinality;

public interface FiniteRowVector<
// Field elements:
F extends SemiRing<F>,
// Cardinality:
C extends Cardinality.Finite,
// Domain:
D extends FiniteColumnVector<F, C, ? extends FiniteRowVector<F, C, ?, ?>, D>,
// Recursive self-type:
S extends FiniteRowVector<F, C, D, S>
>
extends
FiniteVector<F, C, D, S>,
RowVector<F, C, D, S>
{
}
