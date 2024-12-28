/**
 * 
 */
package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.geometry.MetricSpace;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.ordered.Directed;

/**
 * @see https://en.wikipedia.org/wiki/Net_(mathematics)
 */
@FunctionalInterface
public interface Net<
T extends MetricSpace.Me<?, T>,
C extends Cardinality,
D extends Directed.De<D>
>
extends Family<T, C, D>
{
}
