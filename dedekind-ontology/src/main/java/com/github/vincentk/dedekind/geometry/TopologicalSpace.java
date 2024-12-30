package com.github.vincentk.dedekind.geometry;


import com.github.vincentk.dedekind.sets.Element;
import com.github.vincentk.dedekind.sets.Set;

/**
 * @see https://en.wikipedia.org/wiki/Topological_space
 * 
 * @param <M> typically something like the real numbers.
 * @param <T> often equal to {@link M}.
 */
public interface TopologicalSpace<
E extends TopologicalSpace.Me<E>,
M extends TopologicalSpace<E, M>
>
extends
Set<E, M>
{
    interface Me<
    E extends Me<E>
    >
    extends
    Element<E>
    {
    }
}
