package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.sets.Element;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered;

/**
 * @see https://en.wikipedia.org/wiki/Ordered_pair
 * @see https://en.wikipedia.org/wiki/Cartesian_product
 */
public interface Pair<
// Domain:
A extends Element<A>,
// Range:
B extends Element<? super B>,
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
    B extends Element<? super B>
    > 
    (A fst, B snd)
    implements Pair<A, B, Impl<A, B>>
    {
    }

    public interface OrderedUsingFirst<
    // Domain:
    A extends TotallyOrdered.Oe<A>,
    // Range:
    B extends Element<? super B>,
    // Implementation type:
    P extends OrderedUsingFirst<A, B, P>
    >
    extends
    Pair<A, B, P>,
    TotallyOrdered.Oe<P>
    {
	@Override
	default boolean eq(P that) {
	    return fst().eq(that.fst());
	}

	@Override
	default int compareTo(P that) {
	    return fst().compareTo(that.fst());
	}

	record Impl<
	// Domain:
	A extends TotallyOrdered.Oe<A>,
	// Range:
	B extends Element<? super B>
	>(Pair<A, B, ?> inner)
	implements OrderedUsingFirst<A, B, Impl<A, B>>
	{

	    @Override
	    public A fst() {
		return inner.fst();
	    }

	    @Override
	    public B snd() {
		return inner.snd();
	    }

	}
    }
}
