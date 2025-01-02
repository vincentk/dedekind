package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Element;
import com.github.vincentk.dedekind.sets.ordered.DirectedSet;

/**
 * @see https://en.wikipedia.org/wiki/Net_(mathematics)
 * @see https://en.wikipedia.org/wiki/Net_(mathematics)#As_generalization_of_sequences
 */
public interface Net<

// Domain of the net and its elements:
E extends DirectedSet.De<E>,
C extends Cardinality,
D extends DirectedSet<E, C, D>,

// Range of the net:
T extends Element<T>,


//Implementation details:
P extends Pair<E, T, P> & DirectedSet.De<P>,
Z extends Net<E, C, D, T, P, Z>
>
extends
Family<E, C, D, T, P, Z>,
DirectedSet<P, C, Z>
{
}
