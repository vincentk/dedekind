/**
 * 
 */
package com.github.vincentk.dedekind.families;

import java.util.function.Predicate;

import com.github.vincentk.dedekind.algebra.numbers.N;
import com.github.vincentk.dedekind.families.Pair.OrderedUsingFirst.Impl;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Cardinality.Small;
import com.github.vincentk.dedekind.sets.Element;
import com.github.vincentk.dedekind.sets.EmptySet;
import com.github.vincentk.dedekind.sets.Set;
import com.github.vincentk.dedekind.sets.SingletonSet;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered;

/**
 * @see https://en.wikipedia.org/wiki/Sequence#Definition
 * @see https://en.wikipedia.org/wiki/Stream_(computing)
 */
public interface Sequence<

// Domain of the sequence (i.e. a subset of the natural numbers):
Ni extends N.Nat<Ni>,
C extends Cardinality.Countable,
Ns extends N<Ni, C, Ns>,

// Range of the sequence:
Ri extends Element<Ri>,

// Implementing types:
P extends Pair.OrderedUsingFirst<Ni, Ri, P>,
Z extends Sequence<Ni, C, Ns, Ri, P, Z>
>
extends
Net<Ni, C, Ns, Ri, P, Z>,
TotallyOrdered<P, C, Z>
{

    public interface Finite<
    // Domain of the sequence (i.e. a subset of the natural numbers):
    Ni extends N.Nat<Ni>,
    C extends Cardinality.Finite,
    Ns extends N<Ni, C, Ns>,

    // Range of the sequence:
    Ri extends Element<Ri>,

    // Implementing types:
    P extends Pair.OrderedUsingFirst<Ni, Ri, P>,
    Z extends Finite<Ni, C, Ns, Ri, P, Z>
    >
    extends Sequence<Ni, C, Ns, Ri, P, Z>
    {
	record Empty<
	// Domain of the sequence (i.e. a subset of the natural numbers):
	Ni extends N.Nat<Ni>,
	Ns extends N<Ni, Small.Empty, Ns>,
	// Range of the sequence:
	Ri extends Element<Ri>
	>
	()
	implements Finite<Ni, Small.Empty, Ns, Ri, Pair.OrderedUsingFirst.Impl<Ni, Ri>, Empty<Ni, Ns, Ri>> {

	    @Override
	    public EmptySet<Ri> image() {
		return EmptySet.empty();
	    }

	    @Override
	    public Set<Impl<Ni, Ri>, ?, ?> where(Predicate<Impl<Ni, Ri>> Φ) {
		return EmptySet.empty();
	    }
	}

	record SingletonSequence<
	// Domain of the sequence (i.e. a subset of the natural numbers):
	Ni extends N.Nat<Ni>,
	Ns extends N<Ni, Small.One, Ns>,
	// Range of the sequence:
	Ri extends Element<Ri>
	>
	(Ni zero, Ri val)
	implements Finite<Ni, Small.One, Ns, Ri, Pair.OrderedUsingFirst.Impl<Ni, Ri>, SingletonSequence<Ni, Ns, Ri>> {

	    @Override
	    public SingletonSet<Ri, ?> image() {
		return new SingletonSet.Default<>(val());
	    }

	    @Override
	    public Set<Impl<Ni, Ri>, ?, ?> where(Predicate<Impl<Ni, Ri>> Φ) {

		final Impl<Ni, Ri> pr = new Impl<Ni, Ri>(new Pair.Impl<>(zero(), val()));
		if (Φ.test(pr)) {
		    return new SingletonSet.Default<>(pr);
		}
		return EmptySet.empty();
	    }
	}
    }
}
