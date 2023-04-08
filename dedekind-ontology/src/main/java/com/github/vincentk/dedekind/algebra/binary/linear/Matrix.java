package com.github.vincentk.dedekind.algebra.binary.linear;

import com.github.vincentk.dedekind.algebra.unary.SemiRing;
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
K1 extends Array.Vector.Column<F, C1, K1>,

C2 extends Countable,
K2 extends Array.Vector.Column<F, C2, K2>,

V extends Array<F, O, C2, V>,
M extends Matrix<F, O, C1, K1, C2, K2, V, M>
>
extends
// a linear map between column arrays:
LinearMap<F, C1, K1, C2, K2, M>,
// arranged as arrays of arrays (in either row-major or column major order):
Array<V, O, C2, M>
{
}
