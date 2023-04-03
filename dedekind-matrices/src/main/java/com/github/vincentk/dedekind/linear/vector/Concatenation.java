/**
 * 
 */
package com.github.vincentk.dedekind.linear.vector;

import com.github.vincentk.dedekind.algebra.binary.Bracket.Bra;
import com.github.vincentk.dedekind.algebra.binary.Bracket.Ket;
import com.github.vincentk.dedekind.algebra.binary.SemiModule;
import com.github.vincentk.dedekind.algebra.binary.Transposed;
import com.github.vincentk.dedekind.algebra.unary.SemiRing;

/**
 * Vector concatenation arises frequently in practical applications.
 * I am not aware of a corresponding concept in abstract algebra, perhaps
 * because it is possible to express via matrix multiplication + addition.
 * 
 * E.g. [a] + [b] = [a, b]
 * 
 * FIXME: presently overly restrictive PoC.
 * TODO: generalize to vectors with infinite cardinality.
 */
public record Concatenation<
F extends SemiRing<F>,

C1 extends Ket<F, R1, C1>,
R1 extends SemiModule<F, R1> & Bra<F, C1, R1>, //FIXME & Cardinality.Finite,

C2 extends Ket<F, R2, C2>,
R2 extends SemiModule<F, R2> &  Bra<F, C2, R2>
>
(R1 fst, R2 snd)
implements
SemiModule<F, Concatenation<F, C1, R1, C2, R2>>,
Bra<F, Transposed<F, Concatenation<F, C1, R1, C2, R2>>, Concatenation<F, C1, R1, C2, R2>>
{
    // Apparently required to spell this out, else type inference might fail:
    public Concatenation(R1 fst, R2 snd) {
        this.fst = fst;
        this.snd = snd;
    }

    @Override
    public Concatenation<F, C1, R1, C2, R2> mult(F scalar) {
        return new Concatenation<>(fst.mult(scalar), snd.mult(scalar));
    }

    @Override
    public Concatenation<F, C1, R1, C2, R2> plus(Concatenation<F, C1, R1, C2, R2> vector) {
        return new Concatenation<>(fst.plus(vector.fst), snd.plus(vector.snd));
    }

    @Override
    public Transposed<F, Concatenation<F, C1, R1, C2, R2>> transpose() {
        return new Transposed<>(this);
    }

    @Override
    public F dot(Transposed<F, Concatenation<F, C1, R1, C2, R2>> ket) {

        final Concatenation<F, C1, R1, C2, R2> bra = ket.transpose();

        final C1 k1 = bra.fst().transpose();

        final F f1 = fst().dot(k1);

        final C2 k2 = bra.snd().transpose();

        final F f2 = snd().dot(k2);

        return f1.plus(f2);
    }
}
