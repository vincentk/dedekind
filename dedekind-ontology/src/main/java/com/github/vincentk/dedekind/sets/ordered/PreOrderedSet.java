package com.github.vincentk.dedekind.sets.ordered;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Element;
import com.github.vincentk.dedekind.sets.NonEmptySet;
import com.github.vincentk.dedekind.sets.Set;
import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.PreOrder;

/**
 * A {@link Set} with a {@link PreOrder}.
 * 
 * @see https://en.wikipedia.org/wiki/Directed_set
 */
public interface PreOrderedSet<
E extends Element<E>,
C extends Cardinality,

// Implementation details:
O extends PreOrder<E>,
T extends PreOrderedSet<E, C, O, T>
>
extends
NonEmptySet<E, C, T>
{
}