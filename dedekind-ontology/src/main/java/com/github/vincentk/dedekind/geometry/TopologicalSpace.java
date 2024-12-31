package com.github.vincentk.dedekind.geometry;


import com.github.vincentk.dedekind.sets.Cardinality;
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
C extends Cardinality,
M extends TopologicalSpace<E, C, M>
>
extends
Set<E, C, M>
{
    interface Me<
    E extends Me<E>
    >
    extends
    Element<E>
    {
    }
}
