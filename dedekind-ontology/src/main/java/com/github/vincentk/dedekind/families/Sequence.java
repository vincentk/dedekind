/**
 * 
 */
package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Set;
import com.github.vincentk.dedekind.sets.ordered.Interval;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered;

/**
 * @see https://en.wikipedia.org/wiki/Sequence#Definition
 */
@FunctionalInterface
public interface Sequence<
R extends Set.Element<R>,
C extends Cardinality.Countable,

E extends TotallyOrdered.Oe<E>,
D extends TotallyOrdered<E, C, D>,
I extends Interval.HalfOpen.Right<E, D, C, I>
>
extends Family<R, C, E, I>
{
}
