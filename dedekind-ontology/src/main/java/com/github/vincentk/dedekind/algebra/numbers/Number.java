/**
 * 
 */
package com.github.vincentk.dedekind.algebra.numbers;

import com.github.vincentk.dedekind.algebra.structures.SemiRing;
import com.github.vincentk.dedekind.sets.Set;

/**
 * In this context, a number is a member of a set with at least
 * the structure of a semi-ring.
 * 
 * @param <T> self-reference
 */
public interface Number<T extends Number<T>>
extends
SemiRing<T>, Set<T> {
}
