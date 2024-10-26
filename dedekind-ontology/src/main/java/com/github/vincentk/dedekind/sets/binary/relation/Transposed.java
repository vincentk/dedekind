package com.github.vincentk.dedekind.sets.binary.relation;

import com.github.vincentk.dedekind.algebra.structures.Bracket.Bra;
import com.github.vincentk.dedekind.algebra.structures.Bracket.Ket;
import com.github.vincentk.dedekind.algebra.structures.SemiModule;
import com.github.vincentk.dedekind.algebra.structures.SemiRing;
import com.github.vincentk.dedekind.bilinear.OuterProduct;

/**
 * @param <F> the underlying field.
 * @param <D> type of domain / dual vector.
 */
public record Transposed<
// Field elements:
F extends SemiRing<F>,
// Domain:
D extends SemiModule<F, D> & Bra<F, Transposed<F, D>, D>
>
(D val)
implements
SemiModule<F, Transposed<F, D>>,
Ket<F, D, Transposed<F, D>>
{
    @Override
    public D transpose() {
        return val;
    }

    @Override
    public Transposed<F, D> mult(F scalar) {
        return new Transposed<>(val.mult(scalar));
    }

    @Override
    public Transposed<F, D> plus(Transposed<F, D> vector) {
        return new Transposed<>(val.plus(vector.val));
    }

    @Override
    public <
    K1 extends Ket<F, B1, K1>,
    B1 extends Bra<F, K1, B1>
    >
    OuterProduct<F, Transposed<F, D>, D, K1, B1>
    outer(B1 bra) {
        return new OuterProduct<F, Transposed<F, D>, D, K1, B1>(this, bra);
    }

    @Override
    public boolean eq(Transposed<F, D> that) {
	return this == that;
    }
}
