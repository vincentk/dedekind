package com.github.vincentk.dedekind.sets;

/**
 * A countable set. Its elements can be enumerated.
 * 
 * @param <E> element type
 * @param <S> enumeration (e.g. a Supplier<E>, or an Iterable<E>).
 */
public interface CountableSet<E, S> {

    S enumerate();
}
