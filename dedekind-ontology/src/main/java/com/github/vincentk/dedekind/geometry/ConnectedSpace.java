package com.github.vincentk.dedekind.geometry;

import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * @param <E> typically something like the real numbers.
 * @param <M>
 * 
 * @see https://en.wikipedia.org/wiki/Connected_space#Formal_definition
 */
public interface ConnectedSpace<
E extends TopologicalSpace.Me<E>,
C extends Cardinality,
M extends ConnectedSpace<E, C, M>
>
extends
TopologicalSpace<E, C, M>
{
}
