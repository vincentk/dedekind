package com.github.vincentk.dedekind.linear;

import java.util.function.Function;

import com.github.vincentk.dedekind.algebra.structures.Bracket.Ket;
import com.github.vincentk.dedekind.algebra.structures.SemiModule;
import com.github.vincentk.dedekind.algebra.structures.SemiRing;

/**
 * @see https://en.wikipedia.org/wiki/Linear_map
 *
 * @param <F> typically a field.
 * @param <V> domain (a vector space)
 * @param <W> range, co-domain (also a vector space)
 */
public interface LinearMap<
// Field:
F extends SemiRing.SmrE<F>,
// Domain:
V extends Ket<F, ?, V>,
// Range:
W extends Ket<F, ?, W>,
S extends LinearMap<F, V, W, S>
>
extends
Function<V, W>,
SemiModule.SmE<F, S>
{
    @Override
    W apply(V v);
    
    @Override
    S mult(F scalar);
    
    @Override
    S plus(S that);
}
