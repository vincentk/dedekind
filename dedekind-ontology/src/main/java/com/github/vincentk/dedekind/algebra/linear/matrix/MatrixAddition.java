package com.github.vincentk.dedekind.algebra.linear.matrix;

import com.github.vincentk.dedekind.algebra.binary.Bracket.Ket;
import com.github.vincentk.dedekind.algebra.unary.SemiRing;
import com.github.vincentk.dedekind.linear.LinearMap;

/**
 * A lazy / symbolic implementation of matrix addition.
 * 
 * @param <F>
 * @param <C>
 * @param <D>
 */
public record MatrixAddition<
F extends SemiRing<F>,
C extends Ket<F, ?, C>,
D extends Ket<F, ?, D>
>
(
        LinearMap<F, D, C> m1,
        LinearMap<F, D, C> m2
)
implements LinearMap<F, D, C>
{
    @Override
    public C apply(D vector) {

        final var v1 = m1.apply(vector);
        final var v2 = m2.apply(vector);

        return v1.plus(v2);
    }

    @Override
    public MatrixAddition<F, C, D> mult(F scalar) {
        // Distributive law:
        // (A + B) * a = (A * a + B * a)
        return new MatrixAddition<>(m1.mult(scalar), m2.mult(scalar));
    }
}
