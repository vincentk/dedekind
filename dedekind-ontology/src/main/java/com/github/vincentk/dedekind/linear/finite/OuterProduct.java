package com.github.vincentk.dedekind.linear.finite;

import com.github.vincentk.dedekind.algebra.Ring;
import com.github.vincentk.dedekind.algebra.peano.Cardinality;
import com.github.vincentk.dedekind.linear.LinearMap;

/**
 * Outer product / tensor product implementation.
 * 
 * @see https://en.wikipedia.org/wiki/Outer_product
 *
 * @param <R>
 * @param <C>
 * @param <C1>
 * @param <R1>
 * @param <C2>
 * @param <CO>
 * @param <RO>
 */
public final class OuterProduct<
R extends Ring<R>,

C extends Cardinality,
C1 extends FiniteColumnVector<R, C, R1, C1>,
R1 extends FiniteRowVector<R, C, C1, R1>,

C2 extends Cardinality,
CO extends FiniteColumnVector<R, C2, RO, CO>,
RO extends FiniteRowVector<R, C2, CO, RO>
>
implements
LinearMap<R, CO, C1>{
    
    private final C1 column;
    private final RO row;
    
    public OuterProduct(C1 column, RO row) {
        this.column = column;
        this.row = row;
    }

    /**
     * (x y') z = x * (y' z) = x * a = ax .
     */
    @Override
    public C1 apply(CO vec) {
        
        // Associative law:
        final R sc = row.dot(vec);
        
        // Scalar multiplication commutes:
        final C1 col = column.mult(sc);
        
        return col;
    }
}
