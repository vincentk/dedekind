package com.github.vincentk.dedekind.linear.matrix;

import com.github.vincentk.dedekind.algebra.structures.Bracket.Ket;
import com.github.vincentk.dedekind.algebra.structures.SemiRing;

/**
 * A lazy / symbolic implementation of matrix addition.
 * 
 * @param <F>
 * @param <C>
 * @param <D>
 */
public record MatrixAddition<
//Ring:
F extends SemiRing.SmrE<F>,

//Range of the linear map:
C extends Ket<F, ?, C>,

//Domain of linear map:
D extends Ket<F, ?, D>
>
(
        Matrix<F, C, D, ?> m1,
        Matrix<F, C, D, ?> m2
)
implements Matrix<F, C, D, MatrixAddition<F, C, D>>
{
    @Override
    public C apply(D vector) {

        final var v1 = m1.apply(vector);
        final var v2 = m2.apply(vector);

        return v1.plus(v2);
    }

    @Override
    public MatrixAddition<F, D, C> transpose() {
        // (A + B)' = A' + B'
        return new MatrixAddition<>(m1.transpose(), m2.transpose());
    }

    @Override
    public MatrixAddition<F, C, D> mult(F scalar) {
        // Distributive law:
        // (A + B) * a = (A * a + B * a)
        return new MatrixAddition<>(m1.mult(scalar), m2.mult(scalar));
    }

    @Override
    public boolean eq(Matrix<F, C, D, ?> that) {
	return this == that;
    }
}
