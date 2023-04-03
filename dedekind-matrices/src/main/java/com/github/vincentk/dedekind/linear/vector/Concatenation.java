/**
 * 
 */
package com.github.vincentk.dedekind.linear.vector;

import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.algebra.binary.Vector;
import com.github.vincentk.dedekind.bilinear.Bracket.Bra;
import com.github.vincentk.dedekind.bilinear.Bracket.Ket;
import com.github.vincentk.dedekind.bilinear.finite.TransposedRowVector;
import com.github.vincentk.dedekind.sets.Cardinality;

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
C extends Cardinality.Finite,

F1 extends Cardinality.Finite,
C1 extends Ket<F, R1, C1>,
R1 extends Vector<F, F1, R1> & Bra<F, C1, R1>,

F2 extends Cardinality.Finite,
C2 extends Ket<F, R2, C2>,
R2 extends Vector<F, F2, R2> &  Bra<F, C2, R2>
>
(R1 fst, R2 snd)
implements
Vector<F, C, Concatenation<F, C, F1, C1, R1, F2, C2, R2>>,
Bra<F, TransposedRowVector<F, C, Concatenation<F, C, F1, C1, R1, F2, C2, R2>>, Concatenation<F, C, F1, C1, R1, F2, C2, R2>>
{
    // Apparently required to spell this out, else type inference might fail:
    public Concatenation(R1 fst, R2 snd) {
        this.fst = fst;
        this.snd = snd;
    }

    @Override
    public Concatenation<F, C, F1, C1, R1, F2, C2, R2> mult(F scalar) {
        return new Concatenation<>(fst.mult(scalar), snd.mult(scalar));
    }

    @Override
    public Concatenation<F, C, F1, C1, R1, F2, C2, R2> plus(Concatenation<F, C, F1, C1, R1, F2, C2, R2> vector) {
        return new Concatenation<>(fst.plus(vector.fst), snd.plus(vector.snd));
    }

    @Override
    public TransposedRowVector<F, C, Concatenation<F, C, F1, C1, R1, F2, C2, R2>> transpose() {
        return new TransposedRowVector<>(this);
    }

    @Override
    public F dot(TransposedRowVector<F, C, Concatenation<F, C, F1, C1, R1, F2, C2, R2>> ket) {

        final Concatenation<F, C, F1, C1, R1, F2, C2, R2> bra = ket.transpose();

        final C1 k1 = bra.fst().transpose();

        final F f1 = fst().dot(k1);

        final C2 k2 = bra.snd().transpose();

        final F f2 = snd().dot(k2);

        return f1.plus(f2);
    }
}
