/**
 * 
 */
package com.github.vincentk.dedekind.sets;

/**
 * The Axiom of Choice so to speak.
 * 
 * I.e. we can enumerate the elements.
 * 
 * @see https://en.wikipedia.org/wiki/Axiom_of_choice
 */
@FunctionalInterface
public interface AoC<E> {
    E enumeration();
}
