package com.github.vincentk.dedekind.sets;

import java.util.function.Predicate;

import com.github.vincentk.dedekind.families.Sequence;
import com.github.vincentk.dedekind.sets.Cardinality.Small.Empty;

/**
 * There exists exactly one empty set denoted as &empty; .
 * I.e. in java terminology, this is a singleton.
 * 
 * @param <E> ignored
 */
public final class EmptySet<E extends Element<E>>
implements
Set<E, Empty, EmptySet<E>>,
Empty,
FiniteSet.B64<E, Empty, EmptySet<E>>
{

    @Override
    public boolean isEmpty() {
	return true;
    }

    @Override
    public EmptySet<E> intersection(Set<E, ?, ?> that) {
	return this;
    }

    @Override
    public Set<E, ?, ?> union(Set<E, ?, ?> that) {
	return that;
    }

    @Override
    public boolean sub(Set<E, ?, ?> that) {
	return true;
    }

    @Override
    public boolean sup(Set<E, ?, ?> that) {
	return that.isEmpty();
    }

    @Override
    public EmptySet<E> where(Predicate<E> Φ) {
	return this;
    }

    private EmptySet() {}

    private static final EmptySet<?> EMPTY = new EmptySet<>();

    @SuppressWarnings("unchecked")
    public static
    <E extends Element<E>> 
    EmptySet<E>
    empty() {
	return (EmptySet<E>) EMPTY;
    }

    @Override
    public
    Sequence<?, Empty, ?, ?, ?, ?>
    enumerate() {
	return new Sequence.Finite.Empty<>();
    }
}
