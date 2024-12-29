package com.github.vincentk.dedekind.geometry;


import com.github.vincentk.dedekind.algebra.structures.SemiRing;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered;

/**
 * A specialization of the {@link MetricSpace} to sets which are {@link TotallyOrdered}.
 * 
 * @param <C>
 * @param <T>
 * 
 * @see https://en.wikipedia.org/wiki/Number_line
 */
public interface NumberLine<
E extends MetricSpace.Me<E, E> & TotallyOrdered.Oe<E> & SemiRing.SmrE<E>,
C1 extends Cardinality,
T extends NumberLine<E, C1, T>
>
extends
SemiRing<E, T>,
TotallyOrdered<E, C1, T>,
MetricSpace<E, E, T> {
}
