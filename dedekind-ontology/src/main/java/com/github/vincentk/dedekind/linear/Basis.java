/**
 * 
 */
package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.Field;
import com.github.vincentk.dedekind.algebra.SemiRing;
import com.github.vincentk.dedekind.arrays.RandomAccess;
import com.github.vincentk.dedekind.sets.AoC;
import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * An appropriate way to model the notion of a basis would appear to
 * be to declare a distinct type (i.e. defining a set) the elements (instances)
 * of which are vectors.
 * 
 * The elements all have the same cardinality (i.e. the cardinality of the basis).
 * 
 * @see https://en.wikipedia.org/wiki/Basis_(linear_algebra)
 */
public interface Basis<
F extends SemiRing<F>,
C extends Cardinality,
S extends Basis<F, C, S>
>
extends
Vector<F, C, S>
{

    /**
     * Ordered bases allow sequential access of the vector components.
     */
    public interface Ordered<
    F extends Field<F>,
    C extends Cardinality.Countable,
    E extends AoC.Enumeration<F>,
    S extends Ordered<F, C, E, S>
    >
    extends
    Basis<F, C, S>,
    AoC<F, E>
    {

        /**
         * Finite ordered bases allow random access of the vector components.
         */
        public interface Finite<
        F extends Field<F>,
        C extends Cardinality.Finite,
        E extends AoC.Enumeration<F>,
        S extends Finite<F, C, E, S>
        >
        extends
        Ordered<F, C, E, S>,
        RandomAccess<F>
        {
        }
    }
}
