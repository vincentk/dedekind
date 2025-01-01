package com.github.vincentk.dedekind.sets;

import java.util.function.Predicate;

/**
 * The &cup; of two non-empty disjoint sets.
 */
record Union<
E extends Element<E>,
C extends Cardinality
>
(Set<E, ?, ?> fst, Set<E, ?, ?> snd)
implements
NonEmptySet<E, C, Union<E, C>>
{
    @Override
    public Set<E, ?, ?> where(Predicate<E> Φ) {
	return fst().where(Φ).union(snd().where(Φ));
    }
}
