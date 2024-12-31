/**
 * 
 */
package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.structures.Field;
import com.github.vincentk.dedekind.algebra.structures.Vector;
import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * @https://en.wikipedia.org/wiki/Vector_space#Function_spaces
 */
public interface FunctionSpace<
E extends Field.Fe<E>,
C extends Cardinality, 
F extends Vector.Ve<E, F>,
S extends FunctionSpace<E, C, F, S>
>
extends Vector<E, C, F, S>
{

}
