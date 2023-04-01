/**
 * 
 */
package com.github.vincentk.dedekind.linear;

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
//Field:
F extends SemiRing<F>,
C extends Cardinality,
E extends AoC.Enumeration<F>,
S extends Basis<F, C, E, S>
>
extends
Vector<F, S>,
// It is common at this point to introduce the notion of iteration over the vector components:
AoC<F, E>
{
    /**
     * Finite bases allow random access of the vector components.
     */
    public interface Finite<
    F extends SemiRing<F>,
    C extends Cardinality,
    E extends AoC.Enumeration<F>,
    S extends Finite<F, C, E, S>
    >
    extends
    Basis<F, C, E, S>,
    RandomAccess<F>
    {

    }
}
