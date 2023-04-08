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
SemiModule<F, C, S>,
SemiRing<S>,
AoC<F, AoC.Enumeration<F>>
{}
