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
Ri extends Magma.Oe<Ri>,
C extends Cardinality.Finite,

// Declaration of domain elements and the domain:
Ni extends N.Nat<Ni>,
Ns extends N<Ni, C, Ns>,

// Implementation details:
P extends Pair.OrderedUsingFirst<Ni, Ri, P>,
T extends Tuple<Ri, C, Ni, Ns, P, T>
>
extends
Sequence.Finite<Ni, C, Ns, Ri, P, T>
{
}