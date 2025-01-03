package com.github.vincentk.dedekind.sets.binary.function.operation;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Element;
import com.github.vincentk.dedekind.sets.NonEmptySet;
import com.github.vincentk.dedekind.sets.Set;

/**
 * <ol>
 * <li>Let A be some set with an operation f.</li>
 * <li>Let B be some subset of A.</li>
 * <li>B is the closure of A under f if f(x) &isin; B &forall; x &in; B.</li>
 * </ol>
 * Examples:
 * <ol>
 * <li>The natural numbers are the closure of the integers under addition and multiplication.</li>
 * <li>The integers are the closure of the rationals under subtraction.</li>
 * </ol>
 * It appears to be impossible to state this generically (and without a major refactor)
 * in java, as it would require that an interface (in this case {@link Set}) is implemented
 * with two different generic parameters.
 * </p><p>
 * Still, at this point, some specialization can be performed and filling the specifics can
 * presumably be delegated to the implementation.
 * </p>
 * @param <A>
 * @param <B>
 * @param <C>
 * 
 * @see https://en.wikipedia.org/wiki/Closure_(mathematics)
 */
public interface Closure<
C extends Cardinality,
// Superset:
E extends Element<E> & BinaryOperation<E, ?>,
A extends Set<E, ? super C, ?>,
// Subset:
F extends Element<F> & BinaryOperation<F, ?>,
B extends Set<F, ? extends C, ?>,
// Implementing class:
T extends Set<E, C, T> & Closure<C, E, A, F, B, T>
>
extends NonEmptySet<E, C, T>, BinaryOperation<F, T>
{
    // Enable to force a type check:
    B ap(B a);
}
