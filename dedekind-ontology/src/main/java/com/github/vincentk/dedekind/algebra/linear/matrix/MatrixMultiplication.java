package com.github.vincentk.dedekind.algebra.linear.matrix;

import com.github.vincentk.dedekind.algebra.binary.Bracket.Ket;
import com.github.vincentk.dedekind.algebra.unary.SemiRing;
import com.github.vincentk.dedekind.linear.LinearMap;

public record MatrixMultiplication<
F extends SemiRing<F>,
C extends Ket<F, ?, C>,
D extends Ket<F, ?, D>,
E extends Ket<F, ?, E>
>
(
        LinearMap<F, C, E> m1,
        LinearMap<F, E, D> m2
        )
implements LinearMap<F, C, D>
{
    @Override
    public D apply(C vector) {
        return m1.andThen(m2).apply(vector);
    }

    @Override
    public MatrixMultiplication<F, C, D, E> mult(F scalar) {
        // Associative law. Multiplication with a scalar commutes.
        // We can choose any of
        // (A x B) a  = (A * a) x B = A x (b * a)
        return new MatrixMultiplication<>(m1.mult(scalar), m2);
    }
}
