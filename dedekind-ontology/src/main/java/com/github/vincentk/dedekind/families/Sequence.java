/**
 * 
 */
package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.algebra.structures.Magma;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Set;
import com.github.vincentk.dedekind.sets.ordered.ConvexSet;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered;

/**
 * @see https://en.wikipedia.org/wiki/Sequence#Definition
 * @see https://en.wikipedia.org/wiki/Stream_(computing)
 */
public interface Sequence<
R extends Set.Element<R>,
C extends Cardinality.Countable,

E extends TotallyOrdered.Oe<E>,
D extends TotallyOrdered<E, C, D>,
I extends ConvexSet.HalfOpen.Right<E, D, C, I>
>
extends Family<R, C, E, I>
{
    I interval();

    /**
     * @param <R>
     * @param <C>
     * @param <E>
     * @param <D>
     * @param <I>
     * 
     * @see https://en.wikipedia.org/wiki/Sequence#Finite_and_infinite
     * @see https://en.wikipedia.org/wiki/List_(abstract_data_type)
     */
    interface Finite<
    R extends Set.Element<R>,
    C extends Cardinality.Finite,

    E extends TotallyOrdered.Oe<E> & Magma.Oe<E>,
    D extends TotallyOrdered<E, C, D>,
    I extends ConvexSet.Closed<E, D, C, I>
    >
    extends Sequence<R, C, E, D, I>{
	
	E length();
    }
}
