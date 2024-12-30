package com.github.vincentk.dedekind.sets.binary.relation;

import com.github.vincentk.dedekind.sets.Element;

@FunctionalInterface
public interface Relation<
// Domain:
A extends Element<A>,
// Range:
B extends Element<B>,
// Implementation type:
P extends Relation<A, B, P>>
// Namely, it is a subset of the Cartesian product A x B.
//extends Set<P>
{
    public B χ();

    /**
     * @param that
     * @return true exactly if that is related to this.
     */
    default boolean χ(B that) {
	return χ().eq(that);
    }
}
