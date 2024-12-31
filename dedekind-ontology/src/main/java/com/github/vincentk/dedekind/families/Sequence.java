/**
 * 
 */
package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.algebra.numbers.N;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Element;

/**
 * @see https://en.wikipedia.org/wiki/Sequence#Definition
 * @see https://en.wikipedia.org/wiki/Stream_(computing)
 */
public interface Sequence<
R extends Element<R>,
C extends Cardinality.Countable,

E extends N.Nat<E>,
D extends N<E, C, D>
>
extends
Family<R, C, E, D>
{
    /**
     * @param <R>
     * @param <C>
     * @param <E>
     * @param <D>
     * @param <I>
     * 
     * @see https://en.wikipedia.org/wiki/Sequence#Finite_and_infinite
     * @see https://en.wikipedia.org/wiki/List_(abstract_data_type)
     */
    interface Finite<
    R extends Element<R>,
    C extends Cardinality.Finite,

    E extends N.Nat<E>,
    D extends N<E, C, D>
    >
    extends Sequence<R, C, E, D>{

	E length();
    }
}
