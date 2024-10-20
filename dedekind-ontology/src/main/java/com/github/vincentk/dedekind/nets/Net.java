/**
 * 
 */
package com.github.vincentk.dedekind.nets;

import java.util.function.Function;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Set;

/**
 * @see https://en.wikipedia.org/wiki/Net_(mathematics)
 */
@FunctionalInterface
public interface Net<
T,
C extends Cardinality.Countable,
D extends Set.Countable<C, D>
>
extends
Function<D, T>
{
}
