package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Set;

/**
 * @see https://en.wikipedia.org/wiki/Indexed_family
 */
@FunctionalInterface
public interface Family<
// Range:
R extends Set.Element<R>,
C extends Cardinality,
// Domain and its elements:
E extends Set.Element<E>,
D extends Set<E, D>
>
{
    R at(E e);
}
