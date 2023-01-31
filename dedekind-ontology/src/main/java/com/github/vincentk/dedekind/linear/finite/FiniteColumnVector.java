package com.github.vincentk.dedekind.linear.finite;

import com.github.vincentk.dedekind.algebra.Ring;
import com.github.vincentk.dedekind.algebra.peano.Cardinality;
import com.github.vincentk.dedekind.linear.InnerProductSpace.Ket;
import com.github.vincentk.dedekind.linear.LinearMap;

public interface FiniteColumnVector<
// Field elements:
F extends Ring<F>,
// Cardinality:
C extends Cardinality,
// Domain:
D extends FiniteRowVector<F, C, S, D>,
// Recursive self-type:
S extends FiniteColumnVector<F, C, D, S>
>
extends
FiniteVector<F, C, S>,
Ket<F, D, S>
{
    /**
     * Outer product a.k.a. tensor product.
     * 
     * Column vector x row vector -> matrix.
     * 
     * @param column
     * @return
     */
    <
    C2 extends Cardinality,
    CO extends FiniteColumnVector<F, C2, RO, CO>,
    RO extends FiniteRowVector<F, C2, CO, RO>
    >
    LinearMap<F, CO, S> outer(RO row);
}
