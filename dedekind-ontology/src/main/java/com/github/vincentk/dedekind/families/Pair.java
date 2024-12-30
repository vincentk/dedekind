package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.sets.Element;

/**
 * @see https://en.wikipedia.org/wiki/Ordered_pair
 * @see https://en.wikipedia.org/wiki/Cartesian_product
 */
public interface Pair<
// Domain:
A extends Element<A>,
// Range:
B extends Element<B>,
// Implementation type:
P extends Pair<A, B, P>>
extends
Element<P>
{
    A fst();
    B snd();

    @Override
    default boolean eq(P that) {
	return fst().eq(that.fst()) && snd().eq(that.snd());
    }
    
    interface Homogeneous<
    E extends Element<E>,
    H extends Homogeneous<E, H>
    >
    extends Pair<E, E, H>
    {}

    public record Impl<
    A extends Element<A>,
    // Range:
    B extends Element<B>
    > 
    (A fst, B snd)
    implements Pair<A, B, Impl<A, B>>
    {
    }
}
