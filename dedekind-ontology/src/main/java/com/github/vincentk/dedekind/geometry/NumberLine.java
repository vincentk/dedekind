package com.github.vincentk.dedekind.geometry;


import com.github.vincentk.dedekind.algebra.structures.SemiRing;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered;

/**
 * A specialization of a {@link MetricSpace} to sets which are {@link TotallyOrdered}.
 * 
 * @see https://en.wikipedia.org/wiki/Number_line
 * @see https://en.wikipedia.org/wiki/Interval_(mathematics)#Properties
 * 
 * @param <E>
 * @param <C1>
 * @param <T>
 */
public interface NumberLine<
E extends MetricSpace.Me<E, E> & TotallyOrdered.Oe<E> & SemiRing.SmrE<E>,
C1 extends Cardinality,
T extends NumberLine<E, C1, T>
>
extends
SemiRing<E, C1, T>,
TotallyOrdered<E, C1, T>,
MetricSpace<E, C1, E, T>
{
    
    /**
     * An number &isin; {@link NumberLine}.
     * 
     * @param <N>
     */
    interface Number<N extends Number<N>>
    extends
    MetricSpace.Me<N, N>,
    TotallyOrdered.Oe<N>,
    SemiRing.SmrE<N>
    {
	
    }
}
