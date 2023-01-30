package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.Equality;
import com.github.vincentk.dedekind.algebra.Ring;
import com.github.vincentk.dedekind.linear.finite.One;

/**
 * 1 x 1 matrix.
 * 
 * TODO: overly restrictive API for now.
 * I.e. multiplication with a column vector should generally be possible.
 * 
 * Presently used for testing only, hence package private.
 *
 * @param <F>
 */
final class OneByOne<F extends Ring<F> & Equality<F>>
implements Matrix.Square<F, One<F>, One<F>, OneByOne<F>> {

    private final One<F> val;
    
    OneByOne(One<F> val) {
        this.val = val;
    }

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
    

    // Matrix multiplication with a square matrix (too limited, presently):
    @Override
    public OneByOne<F> times(OneByOne<F> that) {
        return new OneByOne<>(apply(that.val));
    }

    @Override
    public OneByOne<F> transpose() {
        return this;
    }

}
