package com.github.vincentk.dedekind.sets;

public interface NonEmptySet<
E extends Set.Element<E>,
T extends Set<E, T>
>
extends Set<E, T>
{
    @Override
    default boolean isEmpty() {
	return false;
    }
}
