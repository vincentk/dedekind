package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.algebra.numbers.N;
import com.github.vincentk.dedekind.algebra.structures.Magma;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.ordered.ConvexSet;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered;

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
D extends TotallyOrdered<E, C, D>,
I extends ConvexSet.Closed<E, D, C, I>,

// Self-reference to the implementation type:
T extends Tuple<M, C, E, D, I, T>
>
extends
Sequence.Finite<E, C, E, D, I>
{
}