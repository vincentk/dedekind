package com.github.vincentk.dedekind.linear.finite;

import com.github.vincentk.dedekind.algebra.Ring;
import com.github.vincentk.dedekind.algebra.peano.Cardinality;
import com.github.vincentk.dedekind.linear.InnerProductSpace.Bra;

public interface FiniteRowVector<
// Field elements:
F extends Ring<F>,
// Cardinality:
C extends Cardinality,
// Domain:
D extends FiniteColumnVector<F, C, S, D>,
// Recursive self-type:
S extends FiniteRowVector<F, C, D, S>
>
extends
FiniteVector<F, C, S>,
Bra<F, D, S>
{
    /**
     * Scalar product.
     * 
     * Row vector x column vector -> scalar.
     * 
     * @param column
     * @return
     */
    F dot(D column);
}
