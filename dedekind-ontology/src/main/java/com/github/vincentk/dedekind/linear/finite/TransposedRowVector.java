package com.github.vincentk.dedekind.linear.finite;

import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.linear.ColumnVector;
import com.github.vincentk.dedekind.linear.LinearMap;
import com.github.vincentk.dedekind.linear.OuterProductSpace.Bra;
import com.github.vincentk.dedekind.linear.OuterProductSpace.Ket;
import com.github.vincentk.dedekind.linear.RowVector;
import com.github.vincentk.dedekind.sets.Cardinality;

public final class TransposedRowVector<
// Field elements:
F extends SemiRing<F>,
C extends Cardinality,
// Domain:
D extends RowVector<F, C, ? extends ColumnVector<F, C, ?, ?>, D>
>
implements
ColumnVector<F, C, D, TransposedRowVector<F, C, D>>
{
    private final D val;

    private TransposedRowVector(D val) {
        this.val = val;
    }

    public static <
    F extends SemiRing<F>,
    C extends Cardinality,
    D extends RowVector<F, C, ? extends ColumnVector<F, C, ?, ?>, D>
    >
    TransposedRowVector<F, C, D>
    transposed(D bra) {
        return new TransposedRowVector<>(bra);
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
    public <
    K1 extends Ket<F, B1, K1>,
    B1 extends Bra<F, K1, B1>>
    LinearMap<F, K1, TransposedRowVector<F, C, D>>
    outer(B1 bra) {
        return new OuterProduct<>(this, bra);
    }
}
