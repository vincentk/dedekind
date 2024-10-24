package com.github.vincentk.dedekind.sets.ordered;

import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * Partially ordered set with joins and meets.
 * 
 * @param <C> cardinality
 * @param <T> implementation type
 * 
 * @see https://en.wikipedia.org/wiki/Lattice_(order)
 */
public interface Lattice<
C extends Cardinality,
T extends Lattice<C, T>
>
extends Directed<C, T>, SemiLattice.Join<C, T>,  SemiLattice.Meet<C, T> {
}