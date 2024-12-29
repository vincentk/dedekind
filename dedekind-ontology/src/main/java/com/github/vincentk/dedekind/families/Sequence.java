/**
 * 
 */
package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.algebra.structures.Magma;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.ordered.Interval;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered;

/**
 * @see https://en.wikipedia.org/wiki/Sequence#Definition
 */
@FunctionalInterface
public interface Sequence<
T extends Magma.Oe<T>,
C extends Cardinality.Countable,

D extends TotallyOrdered.Oe<D>,
I extends Interval<D, ?, ?, ?, I>
>
extends Family<T, C, D, I>
{
}
