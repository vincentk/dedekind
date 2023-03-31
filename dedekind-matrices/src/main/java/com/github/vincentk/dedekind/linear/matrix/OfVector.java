/**
 * 
 */
package com.github.vincentk.dedekind.linear.matrix;

import com.github.vincentk.dedekind.algebra.Equality;
import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.linear.InnerProductSpace.Bra;
import com.github.vincentk.dedekind.linear.InnerProductSpace.Ket;
import com.github.vincentk.dedekind.linear.finite.FiniteRowVector;
import com.github.vincentk.dedekind.linear.finite.One;
import com.github.vincentk.dedekind.linear.finite.TransposedRowVector;
import com.github.vincentk.dedekind.sets.Cardinality;

import static com.github.vincentk.dedekind.linear.finite.TransposedRowVector.transposed;

public abstract sealed class OfVector
<
//Ring:
F extends SemiRing<F>  & Equality<F>,

//Implementation detail:
R1 extends Bra<F, C, R1>,
//Range of the linear map:
C extends Ket<F, R1, C>,

//Implementation detail:
R2 extends Bra<F, D, R2>,
//Domain of linear map:
D extends Ket<F, R2, D>
>
implements Matrix<F, R1, C, R2, D, OfVector<F, R1, C, R2, D>> {

    public final class 
    Row
    <
    R extends FiniteRowVector<F, Cardinality.Finite, TransposedRowVector<F, Cardinality.Finite, R>, R>
    >
    extends OfVector<F, One<F>, One<F>, R, TransposedRowVector<F, Cardinality.Finite, R>>
    {
        private final R row;
        
        public Row(R row) {
            this.row = row;
        }

        @Override
        public Row<R> mult(F scalar) {
            return new Row<>(row.mult(scalar));
        }

        @Override
        public One<F> apply(TransposedRowVector<F, Cardinality.Finite, R> vector) {
            return One.one(row.dot(vector));
        }

        @Override
        public Column<R> transpose() {
            return new Column<>(transposed(row));
        }        
    }
    
    public final class 
    Column
    <
    R extends FiniteRowVector<F, Cardinality.Finite, TransposedRowVector<F, Cardinality.Finite, R>, R>
    >
    extends OfVector<F, R, TransposedRowVector<F, Cardinality.Finite, R>, One<F>, One<F>>
    {
        private final TransposedRowVector<F, Cardinality.Finite, R> column;
        
        public Column(TransposedRowVector<F, Cardinality.Finite, R> column) {
            this.column = column;
        }

        @Override
        public TransposedRowVector<F, Cardinality.Finite, R> apply(One<F> vector) {
            return column.mult(vector.val());
        }

        @Override
        public Column<R> mult(F scalar) {
            return new Column<>(column.mult(scalar));
        }

        @Override
        public Row<R> transpose() {
            return new Row<>(column.transpose());
        }      
    }
}
