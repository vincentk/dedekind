package com.github.vincentk.dedekind.sets.unary.function;

import java.util.function.Function;

import com.github.vincentk.dedekind.sets.Element;

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
A extends Element<A>,
// Range
B extends Element<B>,
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
}
