/**
 * 
 */
package com.github.vincentk.dedekind.nets;

import com.github.vincentk.dedekind.numbers.N;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.SemiRings;
import com.github.vincentk.dedekind.sets.Set;

/**
 * @see https://en.wikipedia.org/wiki/Sequence
 */
@FunctionalInterface
public interface Sequence<
T,
C extends Cardinality.Countable,
D extends Set.Countable<C, D> & SemiRings.Naturals
>
extends Net<T, C, D>
{
    /**
     * Recursive sample implementation of the Fibonacci sequence:
     * 
     * 0, 1, 1, 2, 3, 5, 8, 13, ...
     * 
     * @see https://en.wikipedia.org/wiki/Fibonacci_sequence
     */
    public final class Fibonacci
    implements Sequence<N, Cardinality.Countable, N> {

	@Override
	public N apply(N t) {
	    
	    final var ti = t.integer();
	    
	    switch (ti) {
	    case 0 : return N.ZERO;
	    case 1 : return N.ONE;
	    default : {
		final var f1 = apply(N.nat(ti - 1));
		final var f2 = apply(N.nat(ti - 2));
		return f1.plus(f2);
	    }
	    }
	}
    }
}
