package com.github.vincentk.dedekind.algebra.binary.linear;

import com.github.vincentk.dedekind.algebra.binary.SemiModule;
import com.github.vincentk.dedekind.algebra.unary.SemiRing;

public interface InnerProduct<
R extends SemiRing<R>,
S extends InnerProduct<R, S>
>
extends
SemiModule<R, S> {

    R dot(S that);
}
