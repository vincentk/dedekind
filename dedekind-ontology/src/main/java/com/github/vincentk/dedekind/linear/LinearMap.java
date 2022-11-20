package com.github.vincentk.dedekind.linear;

import java.util.function.Function;

/**
 * @see https://en.wikipedia.org/wiki/Linear_map
 * 
 * @param <V> domain
 * @param <W> range, co-domain
 */
public interface LinearMap<V, W>
extends Function<V, W>
{
	@Override
	W apply(V v);
}
