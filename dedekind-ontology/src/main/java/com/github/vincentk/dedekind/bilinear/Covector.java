/**
 * 
 */
package com.github.vincentk.dedekind.bilinear;

import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.linear.Vector;
import com.github.vincentk.dedekind.sets.Cardinality;

public interface Covector<
F extends SemiRing<F>,
C extends Cardinality,
//The dual:
D extends Vector<F, C, D> & Bracket.Ket<F, S, D>,
//Self-reference:
S extends Covector<F, C, D, S>
>
extends
Vector<F, C, S>,
Bracket.Bra<F, D, S>
{
}
