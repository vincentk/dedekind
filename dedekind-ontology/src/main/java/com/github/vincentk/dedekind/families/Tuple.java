package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.algebra.sets.Rings;
import com.github.vincentk.dedekind.algebra.sets.SemiRings;
import com.github.vincentk.dedekind.algebra.structures.SemiRing;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Set;
import com.github.vincentk.dedekind.sets.ordered.Directed;
import com.github.vincentk.dedekind.sets.ordered.Interval;

/**
 * Tuple ~ a finite sequence of number-like things.
 * 
 * In this case, we use {@link SemiRing} as a stand-in for being something like a number.
 * 
 * https://en.wikipedia.org/wiki/Tuple
 */
public interface Tuple<
T extends Interval<? extends Rings.Integers, ?, ?, ?>,
D extends Set.Finite<D> & Directed<Cardinality.Finite, D> & SemiRings.Naturals,
I extends Tuple<T, D, I>
>
extends
Sequence<T, Cardinality.Finite, D>
{
    int length();
}