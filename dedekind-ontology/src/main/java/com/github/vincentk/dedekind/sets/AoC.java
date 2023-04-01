/**
 * 
 */
package com.github.vincentk.dedekind.sets;

import java.util.Optional;

/**
 * The Axiom of Choice so to speak.
 * 
 * I.e. we can enumerate the elements.
 * 
 * @see https://en.wikipedia.org/wiki/Axiom_of_choice
 */
@FunctionalInterface
public interface AoC<T, E extends AoC.Enumeration<T>> {
    E enumeration();

    @FunctionalInterface
    public interface Enumeration<T> {

        Optional<T> next();
    }
}
