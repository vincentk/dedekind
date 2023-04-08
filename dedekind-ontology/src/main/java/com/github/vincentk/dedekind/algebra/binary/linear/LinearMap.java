/**
 * 
 */
package com.github.vincentk.dedekind.algebra.binary.linear;

import java.util.function.Function;

import com.github.vincentk.dedekind.algebra.binary.Module;
import com.github.vincentk.dedekind.algebra.binary.SemiModule;
import com.github.vincentk.dedekind.algebra.unary.Ring;
import com.github.vincentk.dedekind.algebra.unary.SemiRing;
import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * @see https://en.wikipedia.org/wiki/Linear_map
 *
 * @param <F> typically a field.
 * @param <V> domain (a vector space)
 * @param <W> range, co-domain (also a vector space)
 */
public interface LinearMap<
F extends SemiRing<F>,

C1 extends Cardinality,
// Domain:
V extends SemiModule<F, C1, V>,
// Range:
C2 extends Cardinality,
W extends SemiModule<F, C2, W>,

M extends LinearMap<F, C1, V, C2, W, M>
>
extends
Function<V, W>,
SemiModule<F, C2, M>
{
    @FunctionalInterface
    public interface Default<
    F extends SemiRing<F>,

    C1 extends Cardinality,
    // Domain:
    V extends SemiModule<F, C1, V>,
    // Range:
    C2 extends Cardinality,
    W extends SemiModule<F, C2, W>
    >
    extends LinearMap<F, C1, V, C2, W, Default<F, C1, V, C2, W>>
    {
        @Override
        default Default<F, C1, V, C2, W> zero() {
            return (V v) -> apply(v.zero());
        }

        /**
         * Specialization of {@link Function#compose(Function)}.
         * 
         * @param <U> the domain of the function to be composed
         * @param before the function to be composed
         * @return the composed map
         */
        default 
        <
        C3 extends Cardinality,
        U extends SemiModule<F, C3, U>
        >
        Default<F, C3, U, C2, W> compose(Default<F, C3, U, C1, V> before) {
            return (U u) -> {
                return apply(before.apply(u));
            };
        }

        @Override
        default Default<F, C1, V, C2, W> mult(F scalar) {
            return (V v) -> {
                final V vs = v.mult(scalar);
                return this.apply(vs);
            };
        }

        @Override
        default Default<F,C1, V, C2, W> plus(Default<F, C1, V, C2, W> that) {
            return (V v) -> {
                final W w1 =  this.apply(v), w2 = that.apply(v);
                return w1.plus(w2);
            };
        }
    }

    static <
    F extends Ring<F>,
    C extends Cardinality,
    V extends Module<F, C, V>
    >
    Default<F, C, V, C, V> identity() {
        return (V v) -> v;
    }

    static <
    F extends Ring<F>,
    C1 extends Cardinality,
    V extends Module<F, C1, V>,
    C2 extends Cardinality,
    W extends SemiModule<F, C2, W>
    >
    Default<F, C1, V, C2, W> constant(W w) {
        return (V v) -> w;
    }
}

