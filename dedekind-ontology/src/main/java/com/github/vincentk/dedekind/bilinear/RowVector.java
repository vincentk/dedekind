package com.github.vincentk.dedekind.bilinear;

import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.bilinear.Bracket.Bra;
import com.github.vincentk.dedekind.sets.Cardinality;

public interface RowVector<
// Field elements:
F extends SemiRing<F>,
// Cardinality:
C extends Cardinality,
// Domain:
D extends ColumnVector<F, C, S, D>,
// Recursive self-type:
S extends RowVector<F, C, D, S>
>
extends
Covector<F, C, D, S>,
Bra<F, D, S>
{
}
