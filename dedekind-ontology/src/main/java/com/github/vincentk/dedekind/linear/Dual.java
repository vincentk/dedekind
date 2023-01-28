package com.github.vincentk.dedekind.linear;

/**
 * A notion of a "transpose" / dual.
 * 
 * Presumably, this can be made more precise.
 * 
 * E.g.:
 * Column <-> row vector
 * matrix transpose
 * monad <-> comonad
 * 
 * @param <C>
 */
public interface Dual<C> {
	
	C transpose();
}
