/**
 * 
 */
package com.github.vincentk.dedekind.algebra.binary.linear;

import java.util.function.Function;

import com.github.vincentk.dedekind.algebra.binary.SemiModule;
import com.github.vincentk.dedekind.algebra.unary.SemiRing;

/**
 * @see https://en.wikipedia.org/wiki/Linear_map
 *
 * @param <F> typically a field.
 * @param <V> domain (a vector space)
 * @param <W> range, co-domain (also a vector space)
 */
@FunctionalInterface
public interface Map<
// Field:
F extends SemiRing<F>,
// Domain:
V extends SemiModule<F, V>,
// Range:
W extends SemiModule<F, W>
>
extends
Function<V, W>,
SemiModule<F, Map<F, V, W>>
{   
    default Map<F, V, W> zero() {
        return null; //identity();
    }
    
    /**
     * Specialization of {@link Function#compose(Function)}.
     * 
     * @param <U> the domain of the function to be composed
     * @param before the function to be composed
     * @return the composed map
     */
    default 
    <U extends SemiModule<F, U>>
    Map<F, U, W> compose(Map<F, U, V> before) {
        return (U u) -> {
            return apply(before.apply(u));
        };
    }

    @Override
    default Map<F, V, W> mult(F scalar) {
        return (V v) -> {
            final V vs = v.mult(scalar);
            return this.apply(vs);
        };
    }

    @Override
    default Map<F, V, W> plus(Map<F, V, W> that) {
        return (V v) -> {
            final W w1 =  this.apply(v), w2 = that.apply(v);
            return w1.plus(w2);
        };
    }

    static <
    F extends SemiRing<F>,
    V extends SemiModule<F, V>
    >
    Map<F, V, V> identity() {
        return (V v) -> v;
    }

    static <
    F extends SemiRing<F>,
    V extends SemiModule<F, V>,
    W extends SemiModule<F, W>
    >
    Map<F, V, W> constant(W w) {
        return (V v) -> w;
    }
}

