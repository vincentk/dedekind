package com.github.vincentk.dedekind.linear.finite;

import com.github.vincentk.dedekind.algebra.Equality;
import com.github.vincentk.dedekind.algebra.Ring;
import com.github.vincentk.dedekind.algebra.peano.Peano;
import com.github.vincentk.dedekind.linear.LinearMap;
import com.github.vincentk.dedekind.linear.OuterProductSpace.Bra;
import com.github.vincentk.dedekind.linear.OuterProductSpace.Ket;

/**
 * Vector with just one element.
 * 
 * It is the only vector which can serve as both a row and a column vector.
 * 
 * @param <R> type of ring defining the element type.
 */
public final class One<R extends Ring<R> & Equality<R>>
implements
FiniteColumnVector<R, Peano.Succ<Peano.Zero>, One<R>, One<R>>,
FiniteRowVector<R, Peano.Succ<Peano.Zero>, One<R>, One<R>>,
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
    
    @Override
    public String toString() {
        return "(" + val + ")";
    }

    public static
    <R extends Ring<R> & Equality<R>>
    One<R>
    of(R val) {
        return new One<>(val);
    }

    @Override
    public long cardinality() {
        return 1;
    }

    @Override
    public One<R> transpose() {
        return this;
    }

    @Override
    public R dot(One<R> column) {
        return val.times(column.val);
    }

    @Override
    public <
    K1 extends Ket<R, B1, K1>,
    B1 extends Bra<R, K1, B1>
    >
    LinearMap<R, K1, One<R>> outer(B1 bra) {
        return new OuterProduct<>(this, bra);
    }
}