package com.github.vincentk.dedekind.sets.binary.function.operation;

import com.github.vincentk.dedekind.sets.Element;
import com.github.vincentk.dedekind.sets.unary.function.Lambda;

/**
 * A {@link Lambda} from a set to itself.
 * 
 * @param <A>
 * @param <L>
 * 
 * @see https://en.wikipedia.org/wiki/Operation_(mathematics)
 */
public interface BinaryOperation<
//Domain
A extends Element<A>,
//Implementation
L extends BinaryOperation<A, L>>
extends Lambda<A, A, L>
{
}
