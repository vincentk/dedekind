package com.github.vincentk.dedekind.algebra;

public interface Addition<D, R> {


    R plus(D that);

    // Poor man's operator overloading:
    default R p(D that) {
        return plus(that);
    }
}
