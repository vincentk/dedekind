package com.github.vincentk.dedekind.linear.lazy;

import java.util.stream.Stream;

import com.github.vincentk.dedekind.algebra.Ring;
import com.github.vincentk.dedekind.algebra.peano.Cardinality;
import com.github.vincentk.dedekind.linear.LinearMap;
import com.github.vincentk.dedekind.linear.finite.FiniteColumnVector;
import com.github.vincentk.dedekind.linear.finite.FiniteRowVector;
import com.github.vincentk.dedekind.linear.finite.OuterProduct;

public final class TransposedRowVector<
// Field elements:
F extends Ring<F>,
// Cardinality:
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
    public Stream<F> enumerate() {
        return val.enumerate();
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
    C2 extends Cardinality,
    CO extends FiniteColumnVector<F, C2, RO, CO>,
    RO extends FiniteRowVector<F, C2, CO, RO>
    >
    LinearMap<F, CO, TransposedRowVector<F, C, D>>
    outer(RO row) {
        return new OuterProduct<>(this, row);
    }
}
