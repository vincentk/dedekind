package com.github.vincentk.dedekind.families;

/**
 * @see https://en.wikipedia.org/wiki/Indexed_family
 * 
 * @param <E>
 */
@FunctionalInterface
public interface RandomAccess<E> extends java.util.RandomAccess {
    
    E get(int i);
}
