/**
 * 
 */
package com.github.vincentk.dedekind.linear;

import com.github.vincentk.dedekind.algebra.structures.Field;
import com.github.vincentk.dedekind.algebra.structures.Vector;

/**
 * @https://en.wikipedia.org/wiki/Vector_space#Function_spaces
 */
public interface FunctionSpace<
E extends Field.Fe<E>,
F extends Vector.Ve<E, F>,
S extends FunctionSpace<E, F, S>
>
extends Vector<E, F, S>
{

}
