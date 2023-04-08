package com.github.vincentk.dedekind.linear.finite;

import java.util.Optional;

import com.github.vincentk.dedekind.algebra.binary.Bracket.Bra;
import com.github.vincentk.dedekind.algebra.binary.Bracket.Ket;
import com.github.vincentk.dedekind.algebra.binary.linear.Array;
import com.github.vincentk.dedekind.algebra.peano.Peano.Succ;
import com.github.vincentk.dedekind.algebra.peano.Peano.Zero;
import com.github.vincentk.dedekind.algebra.unary.Ring;
import com.github.vincentk.dedekind.bilinear.OuterProduct;
import com.github.vincentk.dedekind.sets.Cardinality;


/**
 * Vector with just one element.
 * 
 * It is the only vector which can serve as both a row and a column vector.
 * 
 * @param <R> type of ring defining the element type.
 */
public record One<R extends Ring<R>> (R val)
implements
Bra<R, One<R>, One<R>>,
Ket<R, One<R>, One<R>>,
Array<R, Succ<Zero>, One<R>>,
Cardinality.Finite
{
    public static
    <R extends Ring<R>>
    One<R>
    one(R val) {
        return new One<>(val);
    }

    @Override
    public Optional<One<R>> fromEnumeration(Enumeration<R> seq) {
        return seq.next().map(One::one);
    }

    @Override
    public One<R> zero() {
        return one(val.zero());
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
    public One<R> transpose() {
        return this;
    }

    @Override
    public R dot(One<R> col) {
        return val.times(col.val());
    }

    @Override
    public One<R> negate() {
        return one(val.negate());
    }

    @Override
    public <K1 extends Ket<R, B1, K1>, B1 extends Bra<R, K1, B1>> OuterProduct<R, One<R>, One<R>, K1, B1> outer(
            B1 bra) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Enumeration<R> enumeration() {
        return () -> Optional.of(val);
    }

    @Override
    public long cardinality() {
        return 1;
    }
}