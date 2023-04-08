/**
 * 
 */
package com.github.vincentk.dedekind.algebra.binary.linear;

import com.github.vincentk.dedekind.algebra.binary.SemiModule;
import com.github.vincentk.dedekind.algebra.unary.SemiRing;
import com.github.vincentk.dedekind.sets.AoC;
import com.github.vincentk.dedekind.sets.Cardinality;

interface Array<
F extends SemiRing<F>,
O extends MajorOrder,
C extends Cardinality.Countable,
S extends Array<F, O, C, S>
>
extends
SemiRing<S>,
AoC<F, AoC.Enumeration<F>>
{
    interface Vector<
    F extends SemiRing<F>,
    O extends MajorOrder,
    C extends Cardinality.Countable,
    S extends Vector<F, O, C, S>
    >
    extends
    Array<F, O, C, S>,
    SemiModule<F, C, S>
    {
        
        interface Row<
        F extends SemiRing<F>,
        C extends Cardinality.Countable,
        S extends Row<F, C, S>
        >
        extends
        Vector<F, MajorOrder.Row, C, S>,
        InnerProduct<F, C, Column<F, C, ?>, S>
        {
            @Override
            F dot(Column<F, C, ?> that);
        }
        
        interface Column<
        F extends SemiRing<F>,
        C extends Cardinality.Countable,
        S extends Column<F, C, S>
        >
        extends Vector<F, MajorOrder.Column, C, S>{}
    }
}
