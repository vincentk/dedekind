package com.github.vincentk.dedekind.linear.primitives;

import com.github.vincentk.dedekind.algebra.Equality;
import com.github.vincentk.dedekind.algebra.Field;

/**
 * The set of real numbers.
 */
public final class Rs implements Field.Reals<Rs>, Equality<Rs> {

    public static final Rs ZERO = of(0), ONE = of(1), TWO = of(2), THREE = of(3);

    private final double val;

    private Rs(double val) {
        this.val = val;
    }

    public static Rs of(double val) {
        return new Rs(val);
    }

    public double doubleVal() {
        return val;
    }

    @Override
    public Rs plus(Rs that) {
        return of(this.val + that.val);
    }

    @Override
    public Rs minus(Rs that) {
        return of(this.val - that.val);
    }

    @Override
    public Rs times(Rs that) {
        return of(this.val * that.val);
    }

    @Override
    public Rs divide(Rs that) {
        return of(this.val / that.val);
    }

    @Override
    public Rs negate() {
        return of(-val);
    }

    @Override
    public Rs inverse() {
        return of(1.0 / val);
    }

    @Override
    public boolean equals(Rs that) {
        return this.val == that.val;
    }

    @Override
    public boolean equals(Object that) {
        if (that instanceof Rs) {
            return equals((Rs) that);
        }
        return false;
    }

    @Override
    public String toString() {
        return String.valueOf(val);
    }
}
