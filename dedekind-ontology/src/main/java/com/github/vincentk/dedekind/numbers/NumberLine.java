/**
 * 
 */
package com.github.vincentk.dedekind.numbers;

import com.github.vincentk.dedekind.algebra.Ring;
import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Fields;
import com.github.vincentk.dedekind.sets.Set.TotalOrder;

/**
 * A specialization of the {@link MetricSpace} to sets of {@link Number} with a total order.
 * The terminology could be more precise.
 * 
 * @param <C>
 * @param <T>
 * 
 * @see https://en.wikipedia.org/wiki/Number_line
 */
interface NumberLine<
C extends Cardinality,
T extends SemiRing<T> & NumberLine<C, T>>
extends Fields.Reals, Ring<T>, Number<T>, TotalOrder<C, T>, MetricSpace<T, T> {

    @Override
    default T distance(T other) {

        final T diff = minus(other);

        final int cmp = compareTo(other);

        if (cmp >= 0) return diff;

        return diff.neg();
    }
}
