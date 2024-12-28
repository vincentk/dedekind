/**
 * 
 */
package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.algebra.structures.Monoid;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered;

/**
 * @see https://en.wikipedia.org/wiki/Sequence#Definition
 */
@FunctionalInterface
public interface Sequence<
T extends Monoid.P.Pe<T>,
C extends Cardinality.Countable,
D extends TotallyOrdered.Oe<D>
>
extends Family<T, C, D>
{
}
