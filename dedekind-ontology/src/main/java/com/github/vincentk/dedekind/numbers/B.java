/**
 * 
 */
package com.github.vincentk.dedekind.numbers;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Set;

/**
 * Boolean values. Roughly speaking 0 => false, 1 => true.
 */
public interface B extends Number<B>, Set.Po<Cardinality.Finite, B> {

}
