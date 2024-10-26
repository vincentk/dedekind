package com.github.vincentk.dedekind.algebra.structures;

/**
 * A notion of a "transpose" / dual.
 * 
 * Notably:
 * 
 * (x')' = x
 * 
 * @see https://en.wikipedia.org/wiki/Transpose
 * @see https://en.wikipedia.org/wiki/Dual_system#Transposes
 * 
 * E.g.:
 * Column <-> row vector
 * matrix transpose
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
