package com.github.vincentk.dedekind.sets.unary.function.operation;

import com.github.vincentk.dedekind.sets.Set;
import com.github.vincentk.dedekind.sets.unary.function.Lambda;

/**
 * A {@link Lambda} from a set to itself.
 * 
 * @param <A>
 * @param <L>
 * 
 * @see https://en.wikipedia.org/wiki/Operation_(mathematics)
 */
public interface Operation<
//Domain
A extends Set<A>,
//Implementation
L extends Operation<A, L>>
extends Lambda<A, A, L>
{

}
