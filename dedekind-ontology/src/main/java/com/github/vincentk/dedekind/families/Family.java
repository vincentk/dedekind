package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Element;
import com.github.vincentk.dedekind.sets.Set;

/**
 * @see https://en.wikipedia.org/wiki/Indexed_family
 */
@FunctionalInterface
public interface Family<
// Range:
R extends Element<R>,
C extends Cardinality,
// Domain and its elements:
E extends Element<E>,
D extends Set<E, C, D>
>
{
    R at(E e);
}
