package com.github.vincentk.dedekind.algebra.binary.linear;

import java.util.Optional;

import com.github.vincentk.dedekind.algebra.binary.linear.Array.Vector;
import com.github.vincentk.dedekind.algebra.binary.linear.Array.Vector.Col;
import com.github.vincentk.dedekind.algebra.binary.linear.Array.Vector.Row.N;
import com.github.vincentk.dedekind.algebra.binary.linear.MajorOrder.Cols;
import com.github.vincentk.dedekind.algebra.binary.linear.MajorOrder.Rows;
import com.github.vincentk.dedekind.algebra.unary.SemiRing;
import com.github.vincentk.dedekind.sets.AoC;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Cardinality.Countable;

/**
 * A matrix is a special type of linear map,
 * namely a tabular (countable) sort of arrangement of numbers.
 * 
 * @see https://en.wikipedia.org/wiki/Matrix_(mathematics)
 */
public interface Matrix<
F extends SemiRing<F>,
O extends MajorOrder,

C1 extends Countable,
K1 extends Vector<F, Cols, C1, K1>,

C2 extends Countable,
K2 extends Vector<F, Cols, C2, K2>,

V extends Array<F, O, C1, V>,
S extends LinearMap<F, C1, K1, C2, K2, S>
>
extends
// a linear map between column arrays:
LinearMap<F, C1, K1, C2, K2, S>,
// arranged as arrays of arrays (in either row-major or column major order):
Array<V, O, C2, S>
{
    @Override
    K2 apply(K1 ket);

    /**
     * @param i
     * @param j
     * @return matrix element at (i, j)
     */
    default Optional<F> get(long i, long j) {
        return get(i).flatMap(vec -> vec.get(j));
    }

    interface RowFirst<
    F extends SemiRing<F>,
    C1 extends Cardinality.Countable,
    K1 extends Col<F, C1, K1>,
    C2 extends Cardinality.Countable,
    S extends LinearMap<F, C1, K1, C2, Col.Cm<F, C2>, S>
    >
    extends Matrix<F, Rows, C1, K1, C2, Col.Cm<F, C2>, N<F, C1>, S>
    {
        @Override
        default Col.Cm<F, C2> apply(K1 ket) {

            final AoC<F, ?> dots = map(bra -> bra.dot(ket));

            return new Col.Cm<>(dots.enumeration());
        }
    }
}
