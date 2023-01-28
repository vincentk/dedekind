package com.github.vincentk.dedekind.linear.primitives;

import java.util.stream.Stream;

import com.github.vincentk.dedekind.algebra.Equality;
import com.github.vincentk.dedekind.algebra.Ring;
import com.github.vincentk.dedekind.algebra.peano.Peano;
import com.github.vincentk.dedekind.linear.finite.FiniteVector;

/**
 * Vector with just one element.
 * 
 * @param <R> type of ring defining the element type.
 */
public final class One<R extends Ring<R> & Equality<R>>
implements
FiniteVector<R, Peano.Succ<Peano.Zero>, One<R>>,
Equality<One<R>>
{
    protected final R val;

    private One(R val) {
        this.val = val;
    }

    @Override
    public One<R> mult(R scalar) {
        return new One<>(val.times(scalar));
    }

    @Override
    public One<R> plus(One<R> vector) {
        return new One<>(val.plus(vector.val));
    }

    @Override
    public boolean equals(One<R> that) {
        return this.val.equals(that.val);
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean equals(Object other) {
        if (other instanceof One<?>) {
            final One<?> that = (One<?>) other;
            if (this.val.getClass().isAssignableFrom(that.val.getClass())) {
                return equals((One<R>) other);
            }
        }

        return false;
    }

    public static
    <R extends Ring<R> & Equality<R>>
    One<R>
    one(R val) {
        return new One<>(val);
    }

    @Override
    public Stream<R> enumerate() {
        return Stream.of(val);
    }

    @Override
    public long cardinality() {
        return 1;
    }
}