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
E extends Lattice.Le<E>,
C extends Cardinality,
T extends Lattice<E, C, T>
>
extends
PoSet<E, C, T>,
SemiLattice.Join<E, C, T>,  SemiLattice.Meet<E, C, T> {

    interface Le<E extends Le<E>>
    extends
    SemiLattice.Join.Je<E>, SemiLattice.Meet.Me<E>, Directed.De<E>
    {	

    }
}