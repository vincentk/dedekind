package com.github.vincentk.dedekind.linear.finite;

import java.util.stream.Stream;

import com.github.vincentk.dedekind.algebra.Ring;
import com.github.vincentk.dedekind.linear.Vector;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.CountableSet;

public interface FiniteVector<
// Field:
F extends Ring<F>,
C extends Cardinality,
// Self-reference:
V extends FiniteVector<F, C, V>
>
extends
Vector<F, V>,
Cardinality,
CountableSet<F, Stream<F>>
{
}
