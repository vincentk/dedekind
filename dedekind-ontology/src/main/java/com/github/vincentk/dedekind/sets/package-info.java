/**
 * An attempt to spell out type relationships between algebraic structures
 * in set theoretical terms.
 * 
 * <p>
 * As a subtype (in java) corresponds to a sub-set (in set theory), we can
 * make statements such as:
 * "Every field is a ring." (field extends ring)
 * or in a sense equivalently
 * "The set of fields is a sub-set of the set of rings."
 * </p>
 * <p>
 * It is more tricky for methods, which are (roughly speaking) similarly co-variant
 * in their argument type but contra-variant in their return type. I.e. given
 * two pairs of types 
 * A and B with A &isin; B,
 * C and D with C &isin; D,
 * then 
 * </p>
 * <p>
 * f(A) -> C &isin; f(B) -> C // co-variant with regard to domains A, B
 * </p>
 * <p>
 * whereas
 * </p>
 * <p>
 * f(A) -> D &isin; f(A) -> C // contra-variant with regard to ranges C, D.
 * </p>
 * <p>
 * This limits the type of methods which can be declared without breaking the subset
 * relationship to those methods which can meaningfully be defined for all subsets
 * (e.g. equality) and for which the return type is always the same (e.g. boolean, int).
 * </p>
 * @see https://en.wikipedia.org/wiki/Functor#Covariance_and_contravariance
 */
package com.github.vincentk.dedekind.sets;