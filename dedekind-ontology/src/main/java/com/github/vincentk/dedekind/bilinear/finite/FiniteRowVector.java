package com.github.vincentk.dedekind.bilinear.finite;

import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.bilinear.Bracket.Bra;
import com.github.vincentk.dedekind.linear.finite.FiniteVector;
import com.github.vincentk.dedekind.sets.Cardinality;

public interface FiniteRowVector<
// Field elements:
F extends SemiRing<F>,
// Cardinality:
C extends Cardinality.Finite,
// Domain:
D extends FiniteColumnVector<F, C, S, D>,
// Recursive self-type:
S extends FiniteRowVector<F, C, D, S>
>
extends
FiniteVector<F, C, S>,
Bra<F, D, S>
{
}
