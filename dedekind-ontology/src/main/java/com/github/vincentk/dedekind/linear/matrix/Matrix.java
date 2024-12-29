package com.github.vincentk.dedekind.linear.matrix;

import com.github.vincentk.dedekind.algebra.structures.Bracket.Ket;
import com.github.vincentk.dedekind.algebra.structures.Dual;
import com.github.vincentk.dedekind.algebra.structures.SemiRing;
import com.github.vincentk.dedekind.linear.LinearMap;

/**
 * @see https://en.wikipedia.org/wiki/Matrix_(mathematics)
 */
public interface Matrix<
// Ring:
S extends SemiRing.SmrE<S>,

// Range of the linear map:
K1 extends Ket<S, ?, K1>,

// Domain of linear map:
K2 extends Ket<S, ?, K2>,

// Self-reference:
M extends Matrix<S, K1, K2, M>
>
extends
// Any matrix is a linear map from a vector in the domain to the co-domain:
LinearMap<S, K2, K1, Matrix<S, K1, K2, ?>>,
// A transpose is defined:
Dual<Matrix<S, K2, K1, ?>, M>
{
    @Override
    default Matrix<S, K1, K2, ?> plus(Matrix<S, K1, K2, ?> that) {
        return new MatrixAddition<>(this, that);
    }
    
    @Override
    Matrix<S, K2, K1, ?> transpose();
}
