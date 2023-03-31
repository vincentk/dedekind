package com.github.vincentk.dedekind.linear.finite;

import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.linear.Vector;
import com.github.vincentk.dedekind.sets.Cardinality;

public interface FiniteVector<
// Field:
F extends SemiRing<F>,
C extends Cardinality,
//The dual:
D extends FiniteVector<F, C, ? extends FiniteVector<F, C, ?, ?>, ?>,
// Self-reference:
V extends FiniteVector<F, C, D, V>
>
extends
Vector<F, D, V>,
Cardinality.Finite
{
}
