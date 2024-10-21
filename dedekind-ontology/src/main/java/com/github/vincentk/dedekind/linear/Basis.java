/**
 * 
 */
package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.relation.binary.Vector;
import com.github.vincentk.dedekind.relation.binary.homogeneous.Field;
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
F extends Field<F>,
C extends Cardinality,
S extends Basis<F, C, S>
>
extends
Vector<F, C, S>
{
}
