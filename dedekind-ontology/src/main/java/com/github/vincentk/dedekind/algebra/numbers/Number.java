/**
 * 
 */
package com.github.vincentk.dedekind.algebra.numbers;

import com.github.vincentk.dedekind.sets.Set;
import com.github.vincentk.dedekind.sets.relation.binary.homogeneous.SemiRing;

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
