package com.github.vincentk.dedekind.linear.finite;

import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.algebra.binary.Vector;
import com.github.vincentk.dedekind.sets.Cardinality;

public interface FiniteVector<
// Field:
F extends SemiRing<F>,
C extends Cardinality.Finite,
// Self-reference:
V extends FiniteVector<F, C, V>
>
extends
Vector<F, C, V>,
Cardinality.Finite
{
}
