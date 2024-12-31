package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.algebra.numbers.N;
import com.github.vincentk.dedekind.algebra.structures.Magma;
import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * Tuple ~ a finite sequence of number-like things.
 * 
 * In this case, we use {@link Magma} as a stand-in for being something like a number.
 * 
 * @see https://en.wikipedia.org/wiki/Tuple
 */
public interface Tuple<
M extends Magma.Oe<M>,
C extends Cardinality.Finite,

// Declaration of domain elements and the domain:
E extends N.Nat<E>,
D extends N<E, C, D>,

// Self-reference to the implementation type:
T extends Tuple<M, C, E, D, T>
>
extends
Sequence.Finite<E, C, E, D>
{
}