/**
 * 
 */
package com.github.vincentk.dedekind.numbers;

import com.github.vincentk.dedekind.algebra.Equality;
import com.github.vincentk.dedekind.relation.binary.homogeneous.SemiRing;
import com.github.vincentk.dedekind.sets.Set;

/**
 * In this context, a number is a member of a set with at least
 * the structure of a semi-ring.
 * 
 * @param <T> self-reference
 */
public interface Number<T extends Number<T>>
extends
SemiRing<T>, Set<T>, Equality<T> {
}
