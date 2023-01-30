package com.github.vincentk.dedekind.linear;

import java.util.function.Function;

import com.github.vincentk.dedekind.algebra.Ring;

/**
 * @see https://en.wikipedia.org/wiki/Linear_map
 * 
 * @param <V> domain (a vector space)
 * @param <W> range, co-domain (also a vector space)
 */
@FunctionalInterface
public interface LinearMap<
// Field:
F extends Ring<F>,
// Domain:
V extends ColumnVector<F, ?, ?>,
// Range:
W extends ColumnVector<F, ?, ?>
>
extends Function<V, W>
{
    @Override
    W apply(V v);
}
