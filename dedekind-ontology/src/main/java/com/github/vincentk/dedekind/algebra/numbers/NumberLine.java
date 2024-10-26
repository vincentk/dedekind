package com.github.vincentk.dedekind.algebra.numbers;


import com.github.vincentk.dedekind.algebra.sets.Fields;
import com.github.vincentk.dedekind.algebra.structures.MetricSpace;
import com.github.vincentk.dedekind.algebra.structures.Ring;
import com.github.vincentk.dedekind.algebra.structures.SemiRing;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered;

/**
 * A specialization of the {@link MetricSpace} to sets of {@link Number} with a total order.
 * The terminology could be more precise.
 * 
 * @param <C1>
 * @param <T>
 * 
 * @see https://en.wikipedia.org/wiki/Number_line
 */
interface NumberLine<
C1 extends Cardinality,
T extends SemiRing<T> & NumberLine<C1, T>>
extends Fields.Reals, Ring<T>, Number<T>, TotallyOrdered<C1, T>, MetricSpace<T, T> {

    @Override
    default T distance(T other) {

        return minus(other).abs();
    }
}
