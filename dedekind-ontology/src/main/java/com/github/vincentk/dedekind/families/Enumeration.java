package com.github.vincentk.dedekind.families;

import java.util.Optional;

import com.github.vincentk.dedekind.sets.Set;
import com.github.vincentk.dedekind.sets.binary.relation.Pair;

@FunctionalInterface
public interface Enumeration<
T extends Set.Element<T>,
E extends Enumeration<T, E> & Set.Element<E>
>
{
    /**
     * @see https://en.wikipedia.org/wiki/Tuple#Tuples_as_nested_ordered_pairs
     * 
     * @return a pair (head, remainder) exactly if the sequence is non-empty.
     */
    Optional<? extends Pair<T, E, ?>> next();
}
