package com.github.vincentk.dedekind.nets;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.SemiRings;
import com.github.vincentk.dedekind.sets.Set;

/**
 * https://en.wikipedia.org/wiki/Tuple
 */
public interface Tuple<
T,
D extends Set.Finite<D> & SemiRings.Naturals
>
extends Sequence<T, Cardinality.Finite, D>
{

}