package com.github.vincentk.dedekind.linear.matrix;

import java.util.function.Function;

import com.github.vincentk.dedekind.algebra.Module;
import com.github.vincentk.dedekind.algebra.Monoid;
import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.bilinear.Dual;
import com.github.vincentk.dedekind.bilinear.Bracket.Bra;
import com.github.vincentk.dedekind.bilinear.Bracket.Ket;
import com.github.vincentk.dedekind.linear.LinearMap;

/**
 * @see https://en.wikipedia.org/wiki/Matrix_(mathematics)
 */
public interface Matrix<
// Ring:
F extends SemiRing<F>,

// Implementation detail:
R1 extends Bra<F, C, R1>,
// Range of the linear map:
C extends Ket<F, R1, C>,

// Implementation detail:
R2 extends Bra<F, D, R2>,
// Domain of linear map:
D extends Ket<F, R2, D>,

// Self-reference:
M extends Matrix<F, R1, C, R2, D, M>
>
extends
// Addition is supported for matrices with the same domain and range:
Monoid.P<Matrix<F, R1, C, R2, D, ?>>,
// Scalar multiplication (from the right).
Module<F, Matrix<F, R1, C, R2, D, ?>>,
// Any matrix is a linear map from a vector in the domain to the co-domain:
LinearMap<F, D, C>,
// A transpose is defined:
Dual<Matrix<F, R2, D, R1, C, ?>>
{
    @Override
    default Matrix<F, R1, C, R2, D, ?> plus(Matrix<F, R1, C, R2, D, ?> that) {
        return new MatrixAddition<>(this, that);
    }

    @Override
    C apply(D vector);
    
    @Override
    Matrix<F, R1, C, R2, D, ?> mult(F scalar);
    
    @Override
    Matrix<F, R2, D, R1, C, ?> transpose();

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
    Matrix<F, R1, C, R3, E, ?> compose(Matrix<F, R2, D, R3, E, ?> other) {
        return new MatrixMultiplication<>(other, this);
    }
}
