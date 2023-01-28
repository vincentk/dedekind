package com.github.vincentk.dedekind.linear.lazy;

import java.util.stream.Stream;

import org.jooq.lambda.Seq;

import com.github.vincentk.dedekind.algebra.Ring;
import com.github.vincentk.dedekind.algebra.peano.Cardinality;
import com.github.vincentk.dedekind.linear.finite.FiniteColumnVector;
import com.github.vincentk.dedekind.linear.finite.FiniteRowVector;

public final class TransposedRowVector<
// Field elements:
F extends Ring<F>,
// Cardinality:
C extends Cardinality,
// Domain:
D extends FiniteRowVector<F, C, ?, D>
>
implements
FiniteColumnVector<F, C, D, TransposedRowVector<F, C, D>>
{
    private final D val;

    public TransposedRowVector(D val) {
        this.val = val;
    }

    @Override
    public F apply(D v) {
        
        final var s1 = enumerate();
        final var s2 = v.enumerate();
        
        return Seq.seq(s1)
                .zip(s2, (x, y) -> x.times(y))
                .reduce((x, y) -> x.plus(y))
                // N.b. will throw an exception if at least one of the vectors has zero length.
                .orElseThrow();
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
}
