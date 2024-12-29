package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.algebra.sets.SemiRings;
import com.github.vincentk.dedekind.algebra.structures.Magma;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.ordered.Interval;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered;

/**
 * Tuple ~ a finite sequence of number-like things.
 * 
 * In this case, we use {@link Magma} as a stand-in for being something like a number.
 * 
 * @see https://en.wikipedia.org/wiki/Tuple
 */
public interface Tuple<
T extends Magma.Oe<T>,
C extends Cardinality.Finite,

// Declaration of domain elements and the domain:
E extends SemiRings.Naturals & TotallyOrdered.Oe<E>,
D extends Interval.Bounded<E, D, D, C, D>,

// Self-reference to the implementation type:
I extends Tuple<T, C, E, D, I>
>
extends
Sequence<T, C, E, D>
{
    int length();
}