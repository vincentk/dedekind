package com.github.vincentk.dedekind.bilinear;

import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * TODO: generalize to vectors with infinite cardinality.
 * 
 * @param <F> the underlying field.
 * @param <C> vector length.
 * @param <D> type of domain / dual vector.
 */
public class TransposedRowVector<
// Field elements:
F extends SemiRing<F>,
C extends Cardinality.Finite,
// Domain:
D extends RowVector<F, C, TransposedRowVector<F, C, D>, D>
>
implements
ColumnVector<F, C, D, TransposedRowVector<F, C, D>>
{
    private final D val;
    
    public TransposedRowVector(D val) {
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
    public <
    K1 extends Ket<F, B1, K1>,
    B1 extends Bra<F, K1, B1>
    >
    OuterProduct<F, TransposedRowVector<F, C, D>, D, K1, B1>
    outer(B1 bra) {
        return new OuterProduct<F, TransposedRowVector<F, C, D>, D, K1, B1>(this, bra);
    }
}
