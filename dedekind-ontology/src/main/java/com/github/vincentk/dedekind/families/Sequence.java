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
E extends Set.Element<E>,
C extends Cardinality.Countable,

D extends TotallyOrdered.Oe<D>,
I extends Interval<D, ?, ?, C, I>
>
extends Family<E, C, D, I>
{
}
