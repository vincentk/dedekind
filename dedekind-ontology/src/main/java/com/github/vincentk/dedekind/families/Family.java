package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Set;

/**
 * @see https://en.wikipedia.org/wiki/Indexed_family
 */
@FunctionalInterface
public interface Family<
T,
C extends Cardinality,
D extends Set.Element<D>,
I extends Set<D, I>
>
{
    T at(D d);
}
