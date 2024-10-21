package com.github.vincentk.dedekind.algebra;

public interface Addition<D, R> {

    /**
     * @param that
     * @return this + that
     */
    R plus(D that);

    /**
     * Short-hand for {@link #plus(Object)}.
     * 
     * @param that
     * @return this + that
     */
    default R 十(D that) {
        return plus(that);
    }
}
