package com.github.vincentk.dedekind.sets;

import java.util.function.Predicate;

/**
 * The &cup; of two non-empty disjoint sets.
 */
record Union<
E extends Element<E>,
C extends Cardinality
>
(NonEmptySet<E, ?, ?> fst, NonEmptySet<E, ?, ?> snd)
implements
NonEmptySet<E, C, Union<E, C>>
{
    @Override
    public
    Set<E, ?, ?>
    where(Predicate<E> Φ)
    {

	final var w1 = fst().where(Φ);
	final var w2 = snd().where(Φ);

	if (w1.isEmpty()) return w2;
	if (w2.isEmpty()) return w1;

	return fst().where(Φ).union(w2);
    }
}