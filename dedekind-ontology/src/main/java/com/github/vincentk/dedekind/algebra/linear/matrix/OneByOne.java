package com.github.vincentk.dedekind.algebra.linear.matrix;

import java.util.Optional;

import com.github.vincentk.dedekind.algebra.Equality;
import com.github.vincentk.dedekind.algebra.peano.Peano.*;
import com.github.vincentk.dedekind.algebra.unary.Ring;
import com.github.vincentk.dedekind.linear.finite.One;

/**
 * 1 x 1 matrix.
 * 
 * @param <F>
 */
public record OneByOne<F extends Ring<F> & Equality<F>>
(One<F> val)
implements
Square<F, Succ<Zero>, One<F>, OneByOne<F>, OneByOne<F>>
{

    // Scalar multiplication:
    @Override
    public OneByOne<F> mult(F scalar) {
        return new OneByOne<>(val.mult(scalar));
    }

    // Matrix multiplication with a column vector:
    @Override
    public One<F> apply(One<F> vector) {
        return One.one(val.dot(vector));
    }

    @Override
    public OneByOne<F> transpose() {
        return this;
    }

    @Override
    public Enumeration<One<F>> enumeration() {
        return () -> Optional.of(val);
    }
}
