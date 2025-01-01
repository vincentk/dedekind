package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Element;
import com.github.vincentk.dedekind.sets.ordered.Directed;

/**
 * @see https://en.wikipedia.org/wiki/Net_(mathematics)
 * @see https://en.wikipedia.org/wiki/Net_(mathematics)#As_generalization_of_sequences
 */
public interface Net<

// Domain of the net and its elements:
E extends Directed.De<E>,
C extends Cardinality,
D extends Directed<E, C, D>,

// Range of the net:
T extends Element<T>,


//Implementation details:
P extends Pair<E, T, P> & Directed.De<P>,
Z extends Net<E, C, D, T, P, Z>
>
extends
Family<E, C, D, T, P, Z>,
Directed<P, C, Z>
{
}
