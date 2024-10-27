/**
 * 
 */
package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Countable;
import com.github.vincentk.dedekind.algebra.numbers.N;
import com.github.vincentk.dedekind.algebra.sets.Rings;
import com.github.vincentk.dedekind.algebra.sets.SemiRings;
import com.github.vincentk.dedekind.sets.ordered.Directed;
import com.github.vincentk.dedekind.sets.ordered.Interval;

/**
 * @see https://en.wikipedia.org/wiki/Sequence#Definition
 */
@FunctionalInterface
public interface Sequence<
T extends Interval<? extends Rings.Integers, ?, ?, ?>,
C extends Cardinality.Countable,
D extends Countable<C, D> & Directed<C, D> & SemiRings.Naturals
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
    implements Sequence<N, Cardinality.Finite, N> {

	@Override
	public N at(N t) {

	    final var ti = t.integer();
	    if (0 == ti) return N.ZERO;
	    else if (1 == ti)  return N.ONE;
	    else {
		final var f1 = at(N.nat(ti - 1));
		final var f2 = at(N.nat(ti - 2));
		return f1.plus(f2);
	    }
	}
    }
}
