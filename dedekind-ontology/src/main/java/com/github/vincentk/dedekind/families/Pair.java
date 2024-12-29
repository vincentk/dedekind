package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.sets.Set;

/**
 * @see https://en.wikipedia.org/wiki/Ordered_pair
 * @see https://en.wikipedia.org/wiki/Cartesian_product
 */
public interface Pair<
// Domain:
A extends Set.Element<A>,
// Range:
B extends Set.Element<B>,
// Implementation type:
P extends Pair<A, B, P>>
extends
Set.Element<P>
{
    A fst();
    B snd();

    @Override
    default boolean eq(P that) {
	return fst().eq(that.fst()) && snd().eq(that.snd());
    }

    public record Impl<
    A extends Set.Element<A>,
    // Range:
    B extends Set.Element<B>
    > 
    (A fst, B snd)
    implements Pair<A, B, Impl<A, B>>
    {
    }
}
