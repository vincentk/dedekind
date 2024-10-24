package com.github.vincentk.dedekind.linear.finite;

import com.github.vincentk.dedekind.algebra.peano.Peano.Succ;
import com.github.vincentk.dedekind.algebra.peano.Peano.Zero;
import com.github.vincentk.dedekind.sets.binary.relation.Module;
import com.github.vincentk.dedekind.sets.binary.relation.Pair;
import com.github.vincentk.dedekind.sets.binary.relation.Transposed;
import com.github.vincentk.dedekind.sets.binary.relation.Bracket.Bra;
import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.Ring;

/**
 * Vector with exactly two elements.
 * 
 * @param <R> type of ring defining the element type.
 */
public record Two<R extends Ring<R>>
(R fst, R snd)
implements
Pair<R, R, Two<R>>,
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
    public Two<R> mult(R a) {
        return new Two<>(fst.x(a), snd.x(a));
    }

    @Override
    public Two<R> plus(Two<R> that) {
        return new Two<>(fst.十(that.fst), snd.十(that.snd));
    }

    @Override
    public Transposed<R, Two<R>> transpose() {
        return new Transposed<>(this);
    }

    @Override
    public R dot(Transposed<R, Two<R>> col) {

        final Two<R> ct = col.transpose();

        final var y1 = fst.times(ct.fst());

        final var y2 = snd.times(ct.snd());

        return y1.十(y2);
    }

    @Override
    public Two<R> negate() {
        return two(fst.neg(), snd.neg());
    }

    @Override
    public boolean eq(Two<R> that) {
	return equals(that);
    }
}