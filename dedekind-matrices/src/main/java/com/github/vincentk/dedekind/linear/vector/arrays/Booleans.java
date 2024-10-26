package com.github.vincentk.dedekind.linear.vector.arrays;

import java.util.Arrays;
import java.util.function.IntFunction;
import java.util.stream.IntStream;

import com.github.vincentk.dedekind.algebra.numbers.B;
import com.github.vincentk.dedekind.algebra.structures.Bracket.Bra;
import com.github.vincentk.dedekind.algebra.structures.SemiModule;
import com.github.vincentk.dedekind.algebra.structures.Transposed;
import com.github.vincentk.dedekind.arrays.RandomAccess;
import com.github.vincentk.dedekind.sets.Cardinality;

public record Booleans
<
C extends Cardinality.Finite
>
(boolean[] values)
implements
SemiModule<B, Booleans<C>>,
Bra<B, Transposed<B, Booleans<C>>, Booleans<C>>,
RandomAccess<B>
{
    public static Booleans<Cardinality.Finite> booleans(boolean... vals) {
        return new Booleans<>(vals);
    }

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
    public Transposed<B, Booleans<C>> transpose() {
        return new Transposed<>(this);
    }

    @Override
    public B dot(Transposed<B, Booleans<C>> ket) {

        final var that = ket.transpose();

        final IntFunction<B> map = ii -> get(ii).times(that.get(ii));

        final B result = IntStream
                .range(0, values.length)
                .mapToObj(map).reduce(B.bool(false), B::plus);

        return result;
    }

    @Override
    public B get(int i) {
        return B.bool(values[i]);
    }

    @Override
    public String toString() {
        return "Booleans[values=" + Arrays.toString(values) + "]";
    }

    @Override
    public boolean eq(Booleans<C> that) {
        return Arrays.equals(values, that.values);
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean equals(Object other) {

        if (!(other instanceof Booleans)) return false;

        return eq((Booleans<C>)other);
    }
}
