package com.github.vincentk.dedekind.algebra.binary;

import com.github.vincentk.dedekind.algebra.Group;
import com.github.vincentk.dedekind.algebra.Ring;

/**
 * Definition of scalar multiplication over a {@link Ring}.
 *
 * Strictly speaking, this is the definition of a "right module"
 * defining multiplication by a scalar from the right.
 *
 * @see https://en.wikipedia.org/wiki/Module_(mathematics)
 */
public interface Module<R extends Ring<R>, N extends Module<R, N>>
extends SemiModule<R, N>, Group.P<N>
{
    @Override
    N mult(R scalar);

    @Override
    N plus(N module);
}
