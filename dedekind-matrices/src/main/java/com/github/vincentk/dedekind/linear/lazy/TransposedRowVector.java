package com.github.vincentk.dedekind.linear.lazy;

import com.github.vincentk.dedekind.algebra.Ring;
import com.github.vincentk.dedekind.linear.LinearMap;
import com.github.vincentk.dedekind.linear.OuterProductSpace.Bra;
import com.github.vincentk.dedekind.linear.OuterProductSpace.Ket;
import com.github.vincentk.dedekind.linear.finite.FiniteColumnVector;
import com.github.vincentk.dedekind.linear.finite.FiniteRowVector;
import com.github.vincentk.dedekind.linear.finite.OuterProduct;
import com.github.vincentk.dedekind.sets.Cardinality;

public final class TransposedRowVector<
// Field elements:
F extends Ring<F>,
C extends Cardinality,
// Domain:
D extends FiniteRowVector<F, C, TransposedRowVector<F, C, D>, D>
>
implements
FiniteColumnVector<F, C, D, TransposedRowVector<F, C, D>>
{
    private final D val;

    public TransposedRowVector(D val) {

        assert val.cardinality() > 0;

        this.val = val;
    }

    @Override
    public D transpose() {
        return val;
    }

    @Override
    public TransposedRowVector<F, C, D> mult(F scalar) {
        return new TransposedRowVector<>(val.mult(scalar));
    }

    @Override
    public TransposedRowVector<F, C, D> plus(TransposedRowVector<F, C, D> vector) {
        return new TransposedRowVector<>(val.plus(vector.val));
    }

    @Override
    public long cardinality() {
        return val.cardinality();
    }

    @Override
    public <
    K1 extends Ket<F, B1, K1>,
    B1 extends Bra<F, K1, B1>>
    LinearMap<F, K1, TransposedRowVector<F, C, D>>
    outer(B1 bra) {
        return new OuterProduct<>(this, bra);
    }
}
