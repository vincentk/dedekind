package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.algebra.sets.SemiRings;
import com.github.vincentk.dedekind.algebra.structures.Monoid;
import com.github.vincentk.dedekind.algebra.structures.SemiRing;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered;

/**
 * Tuple ~ a finite sequence of number-like things.
 * 
 * In this case, we use {@link SemiRing} as a stand-in for being something like a number.
 * 
 * @see https://en.wikipedia.org/wiki/Tuple
 */
public interface Tuple<
T extends Monoid.P.Pe<T>,
D extends SemiRings.Naturals & TotallyOrdered.Oe<D>,
I extends Tuple<T, D, I>
>
extends
Sequence<T, Cardinality.Finite, D>
{
    int length();
}