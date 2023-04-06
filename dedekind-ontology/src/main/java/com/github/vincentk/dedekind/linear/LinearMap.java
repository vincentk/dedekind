package com.github.vincentk.dedekind.linear;

import java.util.function.Function;

import com.github.vincentk.dedekind.algebra.binary.Bracket.Ket;
import com.github.vincentk.dedekind.algebra.binary.SemiModule;
import com.github.vincentk.dedekind.algebra.linear.matrix.MatrixAddition;
import com.github.vincentk.dedekind.algebra.unary.SemiRing;

/**
 * @see https://en.wikipedia.org/wiki/Linear_map
 *
 * @param <F> typically a field.
 * @param <V> domain (a vector space)
 * @param <W> range, co-domain (also a vector space)
 */
public interface LinearMap<
// Field:
F extends SemiRing<F>,
// Domain:
V extends Ket<F, ?, V>,
// Range:
W extends Ket<F, ?, W>
>
extends
Function<V, W>,
SemiModule<F, LinearMap<F, V, W>>
{
    @Override
    W apply(V v);
    
    @Override
    LinearMap<F, V, W> mult(F scalar);
    
    @Override
    default LinearMap<F, V, W> plus(LinearMap<F, V, W> that) {
        return new MatrixAddition<>(this, that);
    }
}
