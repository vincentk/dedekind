package com.github.vincentk.dedekind.sets.unary.function;

/**
 * @see https://en.wikipedia.org/wiki/Involution_(mathematics)
 * 
 * @param <N>
 */
@FunctionalInterface
public interface Negation<N> {

    N negate();

    default N neg() {
	return negate();
    }
}
