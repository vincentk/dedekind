package com.github.vincentk.dedekind.geometry;

/**
 * @param <E> typically something like the real numbers.
 * @param <M>
 * 
 * @see https://en.wikipedia.org/wiki/Connected_space#Formal_definition
 */
public interface ConnectedSpace<
E extends TopologicalSpace.Me<E>,
M extends ConnectedSpace<E, M>
>
extends
TopologicalSpace<E, M>
{
}
