/**
 * 
 */
package com.github.vincentk.dedekind.bilinear;

import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.linear.Vector;

public interface Covector<
//Field:
F extends SemiRing<F>,
//The dual:
D extends Vector<F, D> & Bracket.Ket<F, S, D>,
//Self-reference:
S extends Covector<F, D, S>
>
extends
Vector<F, S>,
Bracket.Bra<F, D, S>
{
}
