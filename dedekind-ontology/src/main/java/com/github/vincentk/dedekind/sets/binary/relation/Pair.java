package com.github.vincentk.dedekind.sets.binary.relation;

import com.github.vincentk.dedekind.sets.Set;

/**
 * @see https://en.wikipedia.org/wiki/Ordered_pair
 * @see https://en.wikipedia.org/wiki/Cartesian_product
 */
public interface Pair<
// Domain:
A extends Set<A>,
// Range:
B extends Set<B>,
// Implementation type:
P extends Pair<A, B, P>>
extends Relation<A, B, P>
{
    A fst();
    B snd();

    @Override
    default B χ() {
	return snd();
    }

    public record Impl<
    A extends Set<A>,
    // Range:
    B extends Set<B>
    > 
    (A fst, B snd)
    implements Pair<A, B, Impl<A, B>>
    {
    }
}
