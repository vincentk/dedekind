package com.github.vincentk.dedekind.linear.finite;

import com.github.vincentk.dedekind.algebra.peano.Peano.Succ;
import com.github.vincentk.dedekind.algebra.peano.Peano.Zero;
import com.github.vincentk.dedekind.algebra.structures.Module;
import com.github.vincentk.dedekind.algebra.structures.Ring;
import com.github.vincentk.dedekind.sets.binary.relation.Bracket.Bra;
import com.github.vincentk.dedekind.sets.binary.relation.Transposed;

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
    oneOf(R val) {
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
        return oneOf(val.negate());
    }

    @Override
    public boolean eq(One<R> that) {
	return equals(that);
    }
}