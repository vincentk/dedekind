package com.github.vincentk.dedekind.linear.finite;

import com.github.vincentk.dedekind.algebra.peano.Peano.Succ;
import com.github.vincentk.dedekind.algebra.peano.Peano.Zero;
import com.github.vincentk.dedekind.relation.binary.Module;
import com.github.vincentk.dedekind.relation.binary.Transposed;
import com.github.vincentk.dedekind.relation.binary.Bracket.Bra;
import com.github.vincentk.dedekind.relation.binary.homogeneous.Ring;

/**
 * Vector with just one element.
 * 
 * It is the only vector which can serve as both a row and a column vector.
 * 
 * @param <R> type of ring defining the element type.
 */
public record One<R extends Ring<R>> (R val)
implements
Module<R, Succ<Zero>, One<R>>,
Bra<R, Transposed<R, One<R>>, One<R>>
{
    public static
    <R extends Ring<R>>
    One<R>
    one(R val) {
        return new One<>(val);
    }

    @Override
    public One<R> mult(R scalar) {
        return new One<>(val.times(scalar));
    }

    @Override
    public One<R> plus(One<R> vector) {
        return new One<>(val.plus(vector.val));
    }

    @Override
    public Transposed<R, One<R>> transpose() {
        return new Transposed<>(this);
    }

    @Override
    public R dot(Transposed<R, One<R>> col) {
        return val.times(col.transpose().val());
    }

    @Override
    public One<R> negate() {
        return one(val.negate());
    }
}