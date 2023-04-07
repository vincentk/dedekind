package com.github.vincentk.dedekind.algebra.binary.linear;

import java.util.function.Function;

import com.github.vincentk.dedekind.algebra.binary.Dual;
import com.github.vincentk.dedekind.algebra.unary.Ring;
import com.github.vincentk.dedekind.sets.AoC;
import com.github.vincentk.dedekind.sets.AoC.Enumeration;
import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * @see https://en.wikipedia.org/wiki/Matrix_(mathematics)
 */
public interface Matrix<
F extends Ring<F>,

C1 extends Cardinality.Countable,
K1 extends Array<F, C1, K1>,

C2 extends Cardinality.Countable,
K2 extends Array<F, C2, K2>,
// Self-reference:
M extends Matrix<F, C1, K1, C2, K2, M>
>
extends
// Any matrix is a linear map from a vector in the domain to the co-domain:
Map<F, K2, K1>,
// Rows can be enumerated:
AoC<K1, Enumeration<K1>>,
// A transpose is defined:
Dual<Matrix<F, C2, K2, C1, K1, ?>, Matrix<F, C1, K1, C2, K2, ?>>
{
    @Override
    default K1 apply(K2 v) {
        return null;
    }

    @Override
    Matrix<F, C2, K2, C1, K1, ?> transpose();

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
    C3 extends Cardinality.Countable,
    K3 extends Array<F, C3, K3>
    >
    Matrix<F, C1, K1, C3, K3, ?> compose(Matrix<F, C2, K2, C3, K3, ?> other) {
        return null; //new MatrixMultiplication<>(other, this);
    }
}
