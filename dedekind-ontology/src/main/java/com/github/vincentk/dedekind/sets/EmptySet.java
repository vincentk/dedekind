package com.github.vincentk.dedekind.sets;

/**
 * There exists exactly one empty set denoted as &empty; .
 * I.e. in java terminology, this is a singleton.
 * 
 * @param <E> ignored
 */
final class EmptySet<E extends Set.Element<E>>
implements Set<E, EmptySet<E>>{

    @Override
    public boolean isEmpty() {
	return true;
    }

    @Override
    public Set<E, ?> intersection(Set<E, ?> that) {
	return this;
    }

    @Override
    public Set<E, ?> union(Set<E, ?> that) {
	return that;
    }

    @Override
    public boolean sub(Set<E, ?> that) {
	return true;
    }

    @Override
    public boolean sup(Set<E, ?> that) {
	return false;
    }

    private EmptySet() {}

    static final EmptySet<?> EMPTY = new EmptySet<>();
}
