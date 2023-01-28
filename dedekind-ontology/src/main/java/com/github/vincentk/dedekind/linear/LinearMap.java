package com.github.vincentk.dedekind.linear;

import java.util.function.Function;

import com.github.vincentk.dedekind.algebra.Ring;

/**
 * @see https://en.wikipedia.org/wiki/Linear_map
 * 
 * @param <V> domain (a vector space)
 * @param <W> range, co-domain (also a vector space)
 */
public interface LinearMap<
	V extends Vector<F, V>, F extends Ring<F>, 
	W extends Vector<G, W>, G extends Ring<G>
>
extends Function<V, W>
{
	@Override
	W apply(V v);
}
