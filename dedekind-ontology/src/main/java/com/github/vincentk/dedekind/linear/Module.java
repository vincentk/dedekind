package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.Ring;

/**
 * Definition of scalar multiplication over a {@link Ring}.
 * 
 * @see https://en.wikipedia.org/wiki/Module_(mathematics)
 * 
 * @param <R> the scalar ring
 * @param <M> self-type
 */
public interface Module<R extends Ring<R>, M extends Module<R, M>> {
	
	/**
	 * Scalar multiplication.
	 * 
	 * Expectations:
	 * https://en.wikipedia.org/wiki/Module_(mathematics)#Formal_definition
	 * 
	 * @param scalar
	 * @return the scaled module element / vector.
	 */
	M mult(R scalar);
}
