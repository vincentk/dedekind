package com.github.vincentk.dedekind.sets.ordered;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.CountableSet;
import com.github.vincentk.dedekind.sets.NonEmptySet;

/**
 * Here, we define a {@link WellOrderedSet} to be a
 * {@link TotallyOrdered} {@link NonEmptySet} with a least element.
 * 
 * These properties make it a:
 * <ul>
 * <li>a {@link CountableSet} (inductively);</li>
 * <li>a half-open {@link ConvexSet} (by virtue of having a lower bound).</li>
 * <ul>
 * 
 * @see https://en.wikipedia.org/wiki/Well-order
 */
public interface WellOrderedSet<
E extends TotallyOrdered.Oe<E>,
C extends Cardinality.Countable,
T extends WellOrderedSet<E, C, T>
>
extends
TotallyOrdered<E, C, T>,
NonEmptySet<E, C, T>,
CountableSet<E, C, T>,
ConvexSet.HalfOpen.Right<E, T, C, T>
{
    /**
     * {@inheritDoc}
     * 
     * <p>
     * For clarity of exposition and to force a type-check.
     * </p>
     */
    @Override
    E lowerBound();
}
