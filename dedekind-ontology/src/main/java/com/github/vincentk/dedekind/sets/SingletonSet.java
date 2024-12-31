package com.github.vincentk.dedekind.sets;

import java.util.function.Predicate;

import com.github.vincentk.dedekind.algebra.numbers.N;
import com.github.vincentk.dedekind.algebra.numbers.N.Nat;
import com.github.vincentk.dedekind.families.Sequence;
import com.github.vincentk.dedekind.sets.Cardinality.Small;
import com.github.vincentk.dedekind.sets.Cardinality.Small.One;
import com.github.vincentk.dedekind.sets.ordered.Lattice;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered;
import com.github.vincentk.dedekind.sets.unary.function.Lambda;

public interface SingletonSet<E extends Element<E>, S extends SingletonSet<E, S>>
extends
NonEmptySet<E, Small.One, S>,
Small.One,
FiniteSet.B64<E, Small.One, S>
{
    E elem();

    @Override
    default Set<E, ? extends Small.One, ?> where(Predicate<E> Φ) {
	return Φ.test(elem()) ? this : EmptySet.empty();
    }

    @Override
    default boolean sub(Set<E, ?, ?> that) {
	return elem().isin(that);
    }

    @Override
    default long cardinality() {
	return 1;
    }

    @Override
    default
    <
    Z extends Nat<Z>,
    D extends N<Z, One, D>
    >
    Sequence<E, One, Z, D> enumerate(
	    Lambda<Z, E, ?> enumeration) {
	// TODO Auto-generated method stub
	return null;
    }

    public record Default<E extends Element<E>>(E elem)
    implements SingletonSet<E, Default<E>> {

    }

    public interface Ordered<
    E extends TotallyOrdered.Oe<E>,
    S extends Ordered<E, S>
    >
    extends 
    SingletonSet<E, S>,
    TotallyOrdered<E, Small.One, S>,
    Lattice.Bounded<E, Small.One, S>
    {
	@Override
	default E top() {
	    return elem();
	}

	@Override
	default E bottom() {
	    return elem();
	}

	public record Default<E extends TotallyOrdered.Oe<E>>(E elem)
	implements Ordered<E, Default<E>> {
	}
    }
}
