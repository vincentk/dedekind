package com.github.vincentk.dedekind.sets.unary.function;

import java.util.function.Function;

import com.github.vincentk.dedekind.sets.Set;
import com.github.vincentk.dedekind.sets.binary.relation.Pair;
import com.github.vincentk.dedekind.sets.binary.relation.Relation;

/**
 * Unary functions.
 * 
 * @param <A>
 * @param <B>
 * @param <L>
 */
@FunctionalInterface
public interface Lambda<
// Domain
A extends Set.Element<A>,
// Range
B extends Set.Element<B>,
// Implementation
L extends Lambda<A, B, L>
>
/*
extends
Relation<A, B, L>
*/
{
    B ap(A a);

    default Function<A, B> asFunction() {
	return a -> ap(a);
    }

    /**
     * Unary functions correspond to binary relations up
     * to strictness of evaluation order.
     * 
     * @param a
     * @return the corresponding relation.
     */
    default Relation<A, B, ?> asRelation() {
	@SuppressWarnings("unchecked")
	final var a = (A) this;
	return new Pair.Impl<>(a, ap(a));
    }
}
