/**
 * Subset relations between algebraic structures modeled as sub-type relations
 * 
 * Example 1. a ring is a monoid translates as 
 * (Ring instanceof Monoid)
 * 
 * Example 2. the integers are a subset of the reals translates as
 * (Integers instanceof Reals)
 * 
 * Note: as java generics are generally invariant, these sub-type relations
 * can only come as type tags, i.e. interfaces without methods.
 */
package com.github.vincentk.dedekind.algebra.sets;