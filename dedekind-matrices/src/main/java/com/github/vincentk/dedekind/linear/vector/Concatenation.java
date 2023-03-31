/**
 * 
 */
package com.github.vincentk.dedekind.linear.vector;

import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.linear.ColumnVector;
import com.github.vincentk.dedekind.linear.RowVector;
import com.github.vincentk.dedekind.linear.finite.FiniteColumnVector;
import com.github.vincentk.dedekind.linear.finite.FiniteRowVector;
import com.github.vincentk.dedekind.linear.finite.TransposedRowVector;
import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * Vector concatenation arises frequently in practical applications.
 * I am not aware of a corresponding concept in abstract algebra, perhaps
 * because it is possible to express via matrix multiplication + addition.
 * 
 * E.g. [a] + [b] = [a, b]
 */
public record Concatenation<
F extends SemiRing<F>,
C extends Cardinality,

C1 extends FiniteColumnVector<F, ?, ?, C1>,
R1 extends FiniteRowVector<F, ?, C1, R1>,

C2 extends ColumnVector<F, ?, ?, C2>,
R2 extends RowVector<F, ?, C2, R2>
>
(R1 fst, R2 snd)
implements RowVector<F, C, TransposedRowVector<F, C, Concatenation<F, C, C1, R1, C2, R2>>, Concatenation<F, C, C1, R1, C2, R2>>
{
    // Apparently required to spell this out, else type inference might fail:
    public Concatenation(R1 fst, R2 snd) {
        this.fst = fst;
        this.snd = snd;
    }

    public static
    <
    F extends SemiRing<F>,
    C1 extends FiniteColumnVector<F, ?, ?, C1>,
    R1 extends FiniteRowVector<F, ?, C1, R1>,
    C2 extends FiniteColumnVector<F, ?, ?, C2>,
    R2 extends FiniteRowVector<F, ?, C2, R2>
    >
    Concatenation<F, Cardinality.Finite, C1, R1, C2, R2>
    finite(R1 fst, R2 snd) {
        return new Concatenation<>(fst, snd);
    }


    @Override
    public Concatenation<F, C, C1, R1, C2, R2> mult(F scalar) {
        return new Concatenation<>(fst.mult(scalar), snd.mult(scalar));
    }

    @Override
    public Concatenation<F, C, C1, R1, C2, R2> plus(Concatenation<F, C, C1, R1, C2, R2> vector) {
        return new Concatenation<>(fst.plus(vector.fst), snd.plus(vector.snd));
    }

    @Override
    public TransposedRowVector<F, C, Concatenation<F, C, C1, R1, C2, R2>> transpose() {
        return TransposedRowVector.transposed(this);
    }

    @Override
    public F dot(TransposedRowVector<F, C, Concatenation<F, C, C1, R1, C2, R2>> ket) {
        
        final Concatenation<F, C, C1, R1, C2, R2> bra = ket.transpose();
        
        final C1 k1 = bra.fst().transpose();
        
        final F f1 = fst().dot(k1);
        
        final C2 k2 = bra.snd().transpose();
        
        final F f2 = snd().dot(k2);
        
        return f1.plus(f2);
    }
}
