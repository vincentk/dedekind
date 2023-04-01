package com.github.vincentk.dedekind.bilinear;

/**
 * A notion of a "transpose" / dual.
 * 
 * See e.g. 
 * https://en.wikipedia.org/wiki/Transpose
 * https://en.wikipedia.org/wiki/Dual_system#Transposes
 * 
 * E.g.:
 * Column <-> row vector
 * matrix transpose
 * monad <-> comonad
 * 
 * @param <C>
 */
@FunctionalInterface
public interface Dual<C> {

    C transpose();
}
