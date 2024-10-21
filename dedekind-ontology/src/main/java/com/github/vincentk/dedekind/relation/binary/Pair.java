package com.github.vincentk.dedekind.families;

/**
 * @see https://en.wikipedia.org/wiki/Ordered_pair
 * @see https://en.wikipedia.org/wiki/Cartesian_product
 */
public interface Pair<A, B> {
    A fst();
    B snd();
}
