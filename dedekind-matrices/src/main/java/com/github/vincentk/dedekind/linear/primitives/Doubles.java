package com.github.vincentk.dedekind.linear.primitives;

import java.util.Arrays;
import java.util.Optional;

import com.github.vincentk.dedekind.algebra.Equality;
import com.github.vincentk.dedekind.algebra.peano.Peano;
import com.github.vincentk.dedekind.linear.finite.FiniteVector;
import com.github.vincentk.dedekind.numbers.R;
import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * Vector backed by an array of doubles.
 * 
 * @param <R> type of ring defining the element type.
 */
final class Doubles<C extends Cardinality>
implements
FiniteVector<R, Peano.Succ<?>, Doubles<C>>,
Equality<Doubles<C>>
{
    private final double[] val;

    private Doubles(double[] val) {

        assert null != val;

        assert val.length > 0;

        this.val = val;
    }

    @Override
    public Doubles<C> mult(R scalar) {

        final var na = Arrays.copyOf(val, val.length);

        final double sv = scalar.doubleVal();

        for (int ii = 0; ii < val.length; ii++) {
            na[ii] *= sv;
        }

        return new Doubles<>(na);
    }

    @Override
    public Doubles<C> plus(Doubles<C> vector) {

        assert val.length == vector.val.length;

        final var na = Arrays.copyOf(val, val.length);

        final var va = vector.val;
        for (int ii = 0; ii < val.length; ii++) {
            na[ii] += va[ii];
        }

        return new Doubles<>(na);
    }

    @Override
    public boolean equals(Doubles<C> that) {
        return Arrays.equals(this.val, that.val);
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof Doubles) {
            @SuppressWarnings("unchecked")
            final Doubles<C> that = (Doubles<C>) other;
            if (cardinality() == that.cardinality()) {
                return equals(that);
            }
        }

        return false;
    }

    @Override
    public String toString() {
        return Arrays.toString(val);
    }

    public static
    Optional<Doubles<Peano.Succ<?>>>
    doubles(double[] val) {

        return Optional.ofNullable(val)
                .filter(v -> v.length > 0)
                .map(v -> new Doubles<>(v));
    }

    @Override
    public long cardinality() {
        return val.length;
    }
}