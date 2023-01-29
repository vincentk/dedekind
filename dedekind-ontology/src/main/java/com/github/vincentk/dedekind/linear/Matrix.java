package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.MonoidP;
import com.github.vincentk.dedekind.algebra.Ring;

/**
 * @see https://en.wikipedia.org/wiki/Matrix_(mathematics)
 */
public interface Matrix<
// Ring:
F extends Ring<F>,

// Implementation detail:
R1 extends RowVector<F, C, R1>,
// Range of the linear map:
C extends ColumnVector<F, R1, C>,

//Implementation detail:
R2 extends RowVector<F, D, R2>,
// Domain of linear map:
D extends ColumnVector<F, R2, D>,

// Self-reference:
M extends Matrix<F, R1, C, R2, D, M>>
extends
// Addition is supported for matrices of the same type:
MonoidP<M>,
// Any matrix is a linear map from a vector in the domain to the co-domain:
LinearMap<D, C>
{
    @Override
    M plus(M that);
    
    @Override
    C apply(D vector);
    
    
    /**
     * A square matrix.
     */
    public interface Square
    {
        
    }
}
