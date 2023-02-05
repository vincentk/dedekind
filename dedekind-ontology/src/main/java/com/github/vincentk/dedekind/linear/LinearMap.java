package com.github.vincentk.dedekind.linear;

import java.util.function.Function;

import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.linear.InnerProductSpace.Ket;

/**
 * @see https://en.wikipedia.org/wiki/Linear_map
 *
 * @param <F> typically a field.
 * @param <V> domain (a vector space)
 * @param <W> range, co-domain (also a vector space)
 */
@FunctionalInterface
public interface LinearMap<
// Field:
F extends SemiRing<F>,
// Domain:
V extends Ket<F, ?, ?>,
// Range:
W extends Ket<F, ?, ?>
>
extends Function<V, W>
{
    @Override
    W apply(V v);
}
