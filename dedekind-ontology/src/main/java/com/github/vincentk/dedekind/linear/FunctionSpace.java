/**
 * 
 */
package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.Field;
import com.github.vincentk.dedekind.algebra.binary.Vector;
import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * @https://en.wikipedia.org/wiki/Vector_space#Function_spaces
 */
public interface FunctionSpace<
F extends Field<F>,
C extends Cardinality.Uncountable,
S extends FunctionSpace<F, C, S>
>
extends Vector<F, C, S>
{

}
