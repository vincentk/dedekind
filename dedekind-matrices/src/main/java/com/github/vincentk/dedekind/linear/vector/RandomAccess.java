/**
 * 
 */
package com.github.vincentk.dedekind.linear.vector;

@FunctionalInterface
public interface RandomAccess<E> {
    
    E get(int i);
}
