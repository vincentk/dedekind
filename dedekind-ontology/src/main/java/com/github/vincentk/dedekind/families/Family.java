package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Set;

/**
 * @see https://en.wikipedia.org/wiki/Net_(mathematics)
 */
@FunctionalInterface
public interface Family<
T,
C extends Cardinality,
D extends Set.Element<D>
>
{
    T at(D d);
}
