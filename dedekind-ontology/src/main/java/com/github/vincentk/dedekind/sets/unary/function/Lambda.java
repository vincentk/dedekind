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
public interface Λ<
// Domain
A extends Set<A>,
// Range
B extends Set<B>,
// Implementation
L extends Λ<A, B, L>
>
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
    default Relation<A, B, ?> asRelation(A a) {
	return new Pair.Impl<>(a, ap(a));
    }
}
