package com.github.vincentk.dedekind.sets;

import java.util.function.Predicate;

import com.github.vincentk.dedekind.families.Sequence;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered.Oe;
import com.github.vincentk.dedekind.sets.unary.function.Lambda;

public record SingletonSet<E extends Element<E>>
(E elem)
implements
NonEmptySet<E, SingletonSet<E>>,
Finite<E, Cardinality.Finite, SingletonSet<E>>
{
    @Override
    public Set<E, ?> where(Predicate<E> Φ) {
	return Φ.test(elem()) ? this : EmptySet.empty();
    }

    @Override
    public boolean sub(Set<E, ?> that) {
	return elem().isin(that);
    }

    @Override
    public <N extends Oe<N>> Sequence<E, Finite, N, ?, ?> enumerate(Lambda<N, E, ?> enumeration) {
	// TODO Auto-generated method stub
	return null;
    }

    @Override
    public long cardinality() {
	return 1;
    }
}
