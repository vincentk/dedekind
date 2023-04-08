package com.github.vincentk.dedekind.linear.finite;

import com.github.vincentk.dedekind.algebra.binary.Bracket.Bra;
import com.github.vincentk.dedekind.algebra.binary.Module;
import com.github.vincentk.dedekind.algebra.binary.Transposed;
import com.github.vincentk.dedekind.algebra.peano.Peano.Succ;
import com.github.vincentk.dedekind.algebra.peano.Peano.Zero;
import com.github.vincentk.dedekind.algebra.unary.Ring;

/**
 * Vector with just one element.
 * 
 * It is the only vector which can serve as both a row and a column vector.
 * 
 * @param <R> type of ring defining the element type.
 */
public record Two<R extends Ring<R>>
(R x1, R x2)
implements
Module<R, Succ<Succ<Zero>>, Two<R>>,
Bra<R, Transposed<R, Two<R>>, Two<R>>
{
    public static
    <R extends Ring<R>>
    Two<R>
    two(R fst, R snd) {
        return new Two<>(fst, snd);
    }
    
    @Override
    public Two<R> zero() {
        return two(x1().zero(), x2().zero());
    }

    @Override
    public Two<R> mult(R a) {
        return new Two<>(x1.x(a), x2.x(a));
    }

    @Override
    public Two<R> plus(Two<R> that) {
        return new Two<>(x1.p(that.x1), x2.p(that.x2));
    }

    @Override
    public Transposed<R, Two<R>> transpose() {
        return new Transposed<>(this);
    }

    @Override
    public R dot(Transposed<R, Two<R>> col) {

        final Two<R> ct = col.transpose();

        final var y1 = x1.times(ct.x1());

        final var y2 = x2.times(ct.x2());

        return y1.p(y2);
    }

    @Override
    public Two<R> negate() {
        return two(x1.neg(), x2.neg());
    }
}