package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.bilinear.Bracket.Ket;
import com.github.vincentk.dedekind.sets.Cardinality;

public interface ColumnVector<
// Field elements:
F extends SemiRing<F>,
// Cardinality:
C extends Cardinality,
// Domain:
D extends RowVector<F, C, S, D>,
// Recursive self-type:
S extends ColumnVector<F, C, D, S>
>
extends
Vector<F, D, S>,
Ket<F, D, S>
{
}
