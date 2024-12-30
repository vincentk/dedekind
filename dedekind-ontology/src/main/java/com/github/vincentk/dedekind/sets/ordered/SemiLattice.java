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
E extends SemiLattice.Sle<E>,
C extends Cardinality,
T extends SemiLattice<E, C, T>
>
extends
PoSet<E, C, T> 
{
    interface Sle<E extends Pe<E>>
    extends
    PoSet.Pe<E>
    {	

    }

    interface Join<
    E extends Join.Je<E>,
    C extends Cardinality,
    T extends Join<E, C, T>
    >
    extends
    SemiLattice<E, C, T>,
    Directed<E, C, T>
    {
	interface Je<E extends Je<E>>
	extends
	Sle<E>, De<E>
	{
	    /**
	     * @param that
	     * @return the lowest upper bound of this and that.
	     */
	    E join(E that);

	    @SuppressWarnings("unchecked")
	    @Override
	    default E upperBound(E that) {
		return ((E)this).join(that);
	    }
	}
    }

    interface Meet<
    E extends Meet.Me<E>,
    C extends Cardinality,
    T extends Meet<E, C, T>
    >
    extends SemiLattice<E, C, T> {

	interface Me<E extends Me<E>>
	extends
	Sle<E>
	{
	    /**
	     * @param that
	     * @return the greatest lower bound of this and that.
	     */
	    E meet(E that);
	}
    }
}