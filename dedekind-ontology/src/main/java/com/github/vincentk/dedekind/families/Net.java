/**
 * 
 */
package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Set;

/**
 * @see https://en.wikipedia.org/wiki/Net_(mathematics)
 */
@FunctionalInterface
public interface Net<
T,
C extends Cardinality.Countable,
D extends Set.Directed<C, D>
>
{
    T at(D d);
}
