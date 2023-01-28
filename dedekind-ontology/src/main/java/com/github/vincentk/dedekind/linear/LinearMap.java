package com.github.vincentk.dedekind.linear;

import java.util.function.Function;

/**
 * @see https://en.wikipedia.org/wiki/Linear_map
 * 
 * @param <V> domain (a vector space)
 * @param <W> range, co-domain (also a vector space)
 */
public interface LinearMap<
// Domain:
V,
// Range:
W
>
extends Function<V, W>
{
    @Override
    W apply(V v);
}
