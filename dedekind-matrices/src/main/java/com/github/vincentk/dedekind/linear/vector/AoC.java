/**
 * 
 */
package com.github.vincentk.dedekind.linear.vector;

import java.util.stream.BaseStream;

/**
 * The Axiom of Choice so to speak.
 * 
 * I.e. we can enumerate the elements.
 *
 */
public interface AoC<
T,
S extends BaseStream<T, S>
> {
    S enumeration();
}
