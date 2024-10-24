package com.github.vincentk.dedekind.sets.ordered;

import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * A partially ordered set with joins and meets.
 * 
 * @param <C>
 * @param <T>
 * 
 * @see https://en.wikipedia.org/wiki/Semilattice
 * @see https://en.wikipedia.org/wiki/Join_and_meet
 */
public interface SemiLattice<
C extends Cardinality,
T extends SemiLattice<C, T>
>
extends
PoSet<C, T> 
{
    interface Join<
    C extends Cardinality,
    T extends SemiLattice<C, T>
    >
    extends
    SemiLattice<C, T>
    {
	/**
	 * @param that
	 * @return the lowest upper bound of this and that.
	 */
	T join(T that);
    }
    
    interface Meet<
    C extends Cardinality,
    T extends SemiLattice<C, T>
    >
    extends
    SemiLattice<C, T>
    {
	/**
	 * @param that
	 * @return the greatest lower bound of this and that.
	 */
	T meet(T that);
    }
}