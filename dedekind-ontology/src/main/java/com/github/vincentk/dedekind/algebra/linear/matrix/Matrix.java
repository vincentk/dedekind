package com.github.vincentk.dedekind.algebra.linear.matrix;

import java.util.function.Function;

import com.github.vincentk.dedekind.algebra.binary.Bracket.Bra;
import com.github.vincentk.dedekind.algebra.binary.Bracket.Ket;
import com.github.vincentk.dedekind.algebra.binary.Dual;
import com.github.vincentk.dedekind.algebra.unary.SemiRing;
import com.github.vincentk.dedekind.linear.LinearMap;

/**
 * @see https://en.wikipedia.org/wiki/Matrix_(mathematics)
 */
public interface Matrix<
// Ring:
F extends SemiRing<F>,

// Range of the linear map:
K1 extends Ket<F, ?, K1>,

// Domain of linear map:
K2 extends Ket<F, ?, K2>,

// Self-reference:
M extends Matrix<F, K1, K2, M>
>
extends
// Any matrix is a linear map from a vector in the domain to the co-domain:
LinearMap<F, K2, K1>,
// A transpose is defined:
Dual<Matrix<F, K2, K1, ?>, M>
{
    @Override
    default LinearMap<F, K2, K1> plus(LinearMap<F, K2, K1> that) {
        return new MatrixAddition<>(this, that);
    }

    @Override
    Matrix<F, K2, K1, ?> transpose();

    /**
     * Matrix multiplication is the composition of two matrices.
     * 
     * This is a specialization of {@link Function#compose(Function)}.
     * 
     * The range of the argument must match the domain of this matrix.
     * The domain of the argument becomes the domain of the composition.
     * The range of this matrix becomes the range of the composition.
     * 
     * @param other
     * @return the {@link Matrix} corresponding to the composed map.
     */
    default <
    // Domain of the argument becomes the range of the result:
    R3 extends Bra<F, E, R3>,
    E extends Ket<F, R3, E>
    >
    Matrix<F, K1, E, ?> compose(Matrix<F, K2, E, ?> other) {
        return null; //new MatrixMultiplication<>(other, this);
    }
}
