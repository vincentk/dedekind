/**
 * 
 */
package com.github.vincentk.dedekind.algebra.binary.linear;

/**
 * Type tags for row-first and column-first major order.
 * 
 * @see https://en.wikipedia.org/wiki/Row-_and_column-major_order
 */
public sealed interface MajorOrder permits MajorOrder.Rows, MajorOrder.Cols {
    
    final class Rows implements MajorOrder {}
    
    final class Cols implements MajorOrder {}
}
