package com.github.vincentk.dedekind.linear.matrix;

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

// Implementation detail:
B1 extends Bra<F, K1, B1>,
// Range of the linear map:
K1 extends Ket<F, B1, K1>,

// Implementation detail:
B2 extends Bra<F, K2, B2>,
// Domain of linear map:
K2 extends Ket<F, B2, K2>,

// Self-reference:
M extends Matrix<F, B1, K1, B2, K2, M>
>
extends
// Any matrix is a linear map from a vector in the domain to the co-domain:
LinearMap<F, K2, K1, Matrix<F, B1, K1, B2, K2, ?>>,
// A transpose is defined:
Dual<Matrix<F, B2, K2, B1, K1, ?>, M>
{
    @Override
    default Matrix<F, B1, K1, B2, K2, ?> plus(Matrix<F, B1, K1, B2, K2, ?> that) {
        return new MatrixAddition<>(this, that);
    }
    
    @Override
    Matrix<F, B2, K2, B1, K1, ?> transpose();

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
    Matrix<F, B1, K1, R3, E, ?> compose(Matrix<F, B2, K2, R3, E, ?> other) {
        return new MatrixMultiplication<>(other, this);
    }
}
