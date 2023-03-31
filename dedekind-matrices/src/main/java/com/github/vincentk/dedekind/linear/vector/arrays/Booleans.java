/**
 * 
 */
package com.github.vincentk.dedekind.linear.vector.arrays;

import java.util.Arrays;
import java.util.function.IntFunction;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import com.github.vincentk.dedekind.linear.finite.FiniteRowVector;
import com.github.vincentk.dedekind.linear.finite.TransposedRowVector;
import com.github.vincentk.dedekind.linear.vector.AoC;
import com.github.vincentk.dedekind.linear.vector.RandomAccess;
import com.github.vincentk.dedekind.numbers.B;
import com.github.vincentk.dedekind.sets.Cardinality;

public record Booleans
<
C extends Cardinality.Finite
>
(boolean[] values)
implements
FiniteRowVector<
B,
C,
TransposedRowVector<B, C, Booleans<C>>,
Booleans<C>>,
RandomAccess<B>,
AoC<B, Stream<B>>
{
    @Override
    public Booleans<C> mult(B scalar) {

        final boolean[] b1 = Arrays.copyOf(values, values.length);

        for (int ii = 0; ii < values.length; ii++) {
            b1[ii] = scalar.times(get(ii)).bool();
        }

        return new Booleans<C>(b1);
    }

    @Override
    public Booleans<C> plus(Booleans<C> vector) {

        final boolean[] b1 = Arrays.copyOf(values, values.length);

        for (int ii = 0; ii < values.length; ii++) {
            b1[ii] = get(ii).times(vector.get(ii)).bool();
        }

        return new Booleans<C>(b1);
    }

    @Override
    public TransposedRowVector<B, C, Booleans<C>> transpose() {
        return TransposedRowVector.transposed(this);
    }

    @Override
    public long cardinality() {
        return values.length;
    }

    @Override
    public B dot(TransposedRowVector<B, C, Booleans<C>> ket) {

        final var that = ket.transpose();

        final IntFunction<B> map = ii -> get(ii).times(that.get(ii));

        final B result = IntStream
                .range(0, values.length)
                .mapToObj(map).reduce(B.of(false), B::plus);

        return result;
    }

    @Override
    public B get(int i) {
        return B.of(values[i]);
    }

    @Override
    public Stream<B> enumeration() {
        return IntStream
                .range(0, values.length)
                .mapToObj(idx -> get(idx));
    }
}
