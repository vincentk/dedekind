package com.github.vincentk.dedekind.sets;

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
}
