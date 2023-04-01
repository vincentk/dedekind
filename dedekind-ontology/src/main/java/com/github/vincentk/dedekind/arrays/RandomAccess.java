/**
 * 
 */
package com.github.vincentk.dedekind.arrays;

@FunctionalInterface
public interface RandomAccess<E> extends java.util.RandomAccess {
    
    E get(int i);
}
