package com.github.vincentk.dedekind.algebra.binary.linear;

import com.github.vincentk.dedekind.algebra.binary.SemiModule;
import com.github.vincentk.dedekind.algebra.unary.SemiRing;
import com.github.vincentk.dedekind.sets.Cardinality;

public interface InnerProduct<
R extends SemiRing<R>,
C extends Cardinality,
D,
S extends InnerProduct<R, C, D, S>
>
extends
SemiModule<R, C, S> {

    R dot(D that);
}
