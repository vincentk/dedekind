package com.github.vincentk.dedekind.linear.finite;

import com.github.vincentk.dedekind.algebra.Equality;
import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.algebra.peano.Peano.Succ;
import com.github.vincentk.dedekind.algebra.peano.Peano.Zero;
import com.github.vincentk.dedekind.bilinear.OuterProductSpace.Bra;
import com.github.vincentk.dedekind.bilinear.OuterProductSpace.Ket;
import com.github.vincentk.dedekind.linear.LinearMap;
import com.github.vincentk.dedekind.bilinear.OuterProduct;

/**
 * Vector with just one element.
 * 
 * It is the only vector which can serve as both a row and a column vector.
 * 
 * @param <R> type of ring defining the element type.
 */
public record One<R extends SemiRing<R> & Equality<R>> (R val)
implements
FiniteColumnVector<R, Succ<Zero>, One<R>, One<R>>,
FiniteRowVector<R, Succ<Zero>, One<R>, One<R>>,
Equality<One<R>>
{
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

    public static
    <R extends SemiRing<R> & Equality<R>>
    One<R>
    one(R val) {
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