package com.github.vincentk.dedekind.sets;

non-sealed
public interface NonEmptySet<
E extends Element<E>,
C extends Cardinality,
T extends Set<E, C, T>
>
extends Set<E, C, T>
{
    @Override
    default boolean isEmpty() {
	return false;
    }

    /**
     * @param that
     * @return this &cup; that
     */
    @Override
    default Set<E, ?, ?> union(Set<E, ?, ?> that) {

	if (isEmpty()) return that;
	if (that.isEmpty()) return this;

	final var cpl = complement(that);
	if (cpl.isEmpty()) return that;

	// TODO: the cardinality of the union is actually not known
	// at this point:
	return new Union<E, C>(this, (NonEmptySet<E, ?, ?>) cpl);
    }
}
