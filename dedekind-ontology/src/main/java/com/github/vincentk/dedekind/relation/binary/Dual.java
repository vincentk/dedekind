package com.github.vincentk.dedekind.relation.binary;

/**
 * A notion of a "transpose" / dual.
 * 
 * Notably:
 * 
 * (x')' = x
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
 * @param <C> the dual type
 */
@FunctionalInterface
public interface Dual<C, D extends Dual<C, D>> {

    /**
     * @return the transposed object.
     */
    C transpose();
}
