package com.github.vincentk.dedekind.bilinear.finite;

import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.bilinear.ColumnVector;
import com.github.vincentk.dedekind.linear.finite.FiniteVector;
import com.github.vincentk.dedekind.sets.Cardinality;

public interface FiniteColumnVector<
// Field elements:
F extends SemiRing<F>,
// Cardinality:
C extends Cardinality.Finite,
// Domain:
D extends FiniteRowVector<F, C, S, D>,
// Recursive self-type:
S extends FiniteColumnVector<F, C, D, S>
>
extends
FiniteVector<F, C, S>,
ColumnVector<F, C, D, S>
{
}
