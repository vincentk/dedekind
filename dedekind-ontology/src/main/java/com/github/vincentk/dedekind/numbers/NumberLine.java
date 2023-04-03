/**
 * 
 */
package com.github.vincentk.dedekind.numbers;

import com.github.vincentk.dedekind.algebra.unary.Ring;
import com.github.vincentk.dedekind.algebra.unary.SemiRing;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Fields;
import com.github.vincentk.dedekind.sets.Set.TotalOrder;

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
extends Fields.Reals, Ring<T>, Number<T>, TotalOrder<C1, T>, MetricSpace<T, T> {

    @Override
    default T distance(T other) {

        return minus(other).abs();
    }
}
