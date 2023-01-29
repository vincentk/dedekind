package com.github.vincentk.dedekind.algebra;

public interface Multiplication<D, R> {

    R times(D that);

    // Poor man's operator overloading:
    default R x(D that) {
        return times(that);
    }
}
