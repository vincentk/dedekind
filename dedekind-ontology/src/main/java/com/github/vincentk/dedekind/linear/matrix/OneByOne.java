package com.github.vincentk.dedekind.linear.matrix;

import com.github.vincentk.dedekind.algebra.Equality;
import com.github.vincentk.dedekind.algebra.binary.Transposed;
import com.github.vincentk.dedekind.algebra.unary.Ring;
import com.github.vincentk.dedekind.linear.finite.One;

/**
 * 1 x 1 matrix.
 * 
 * TODO: overly restrictive API for now.
 * I.e. multiplication with a column vector should generally be possible.
 * 
 * Presently used for testing only.
 *
 * @param <F>
 */
public record OneByOne<F extends Ring<F> & Equality<F>>
(One<F> val)
implements
Square<F, Transposed<F, One<F>>, OneByOne<F>, OneByOne<F>>
{

    // Scalar multiplication:
    @Override
    public OneByOne<F> mult(F scalar) {
        return new OneByOne<>(val.mult(scalar));
    }

    // Matrix multiplication with a column vector:
    @Override
    public Transposed<F, One<F>> apply(Transposed<F, One<F>> vector) {
        return One.one(val.dot(vector)).transpose();
    }

    @Override
    public OneByOne<F> transpose() {
        return this;
    }
}
