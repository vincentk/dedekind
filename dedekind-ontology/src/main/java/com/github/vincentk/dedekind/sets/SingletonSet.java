package com.github.vincentk.dedekind.sets;

import java.util.function.Predicate;

import com.github.vincentk.dedekind.algebra.numbers.N.Nat;
import com.github.vincentk.dedekind.families.Sequence;
import com.github.vincentk.dedekind.sets.ordered.Lattice;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered;
import com.github.vincentk.dedekind.sets.unary.function.Lambda;

public interface SingletonSet<E extends Element<E>, S extends SingletonSet<E, S>>
extends
NonEmptySet<E, S>,
FiniteSet.B64<E, Cardinality.Finite.PowerOfTwo.B64, S>
{
    E elem();

    @Override
    default Set<E, ?> where(Predicate<E> Φ) {
	return Φ.test(elem()) ? this : EmptySet.empty();
    }

    @Override
    default boolean sub(Set<E, ?> that) {
	return elem().isin(that);
    }

    @Override
    default long cardinality() {
	return 1;
    }

    @Override
    default
    <D extends Nat<D>>
    Sequence<E, Cardinality.Finite.PowerOfTwo.B64, D, ?, ?> enumerate(
	    Lambda<D, E, ?> enumeration) {
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
    TotallyOrdered<E, Cardinality.Finite, S>,
    Lattice.Bounded<E, Cardinality.Finite, S>
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
