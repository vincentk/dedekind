/**
 * 
 */
package com.github.vincentk.dedekind.nets;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Rings;
import com.github.vincentk.dedekind.sets.Set;

/**
 * The Axiom of Choice so to speak.
 * 
 * I.e. we can enumerate the elements.
 * 
 * @see https://en.wikipedia.org/wiki/Axiom_of_choice
 * @see https://en.wikipedia.org/wiki/Sequence
 */
@FunctionalInterface
public interface Sequence<
T,
C extends Cardinality.Countable,
D extends Set.Countable<C, D> & Rings.Integers
>
extends Net<T, C, D>
{

}
