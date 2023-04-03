package com.github.vincentk.dedekind.linear;

import java.util.function.Function;

import com.github.vincentk.dedekind.algebra.Monoid;
import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.algebra.binary.SemiModule;
import com.github.vincentk.dedekind.bilinear.Bracket.Ket;

/**
 * @see https://en.wikipedia.org/wiki/Linear_map
 *
 * @param <F> typically a field.
 * @param <V> domain (a vector space)
 * @param <W> range, co-domain (also a vector space)
 */
public interface LinearMap<
// Field:
F extends SemiRing<F>,
// Domain:
V extends Ket<F, ?, V>,
// Range:
W extends Ket<F, ?, W>,
S extends LinearMap<F, V, W, S>
>
extends
Function<V, W>,
//Scalar multiplication (from the right).
SemiModule<F, S>,
//Addition is supported for linear maps with the same domain and range:
Monoid.P<S>
{
    @Override
    W apply(V v);
    
    @Override
    S mult(F scalar);
    
    @Override
    S plus(S that);
}
