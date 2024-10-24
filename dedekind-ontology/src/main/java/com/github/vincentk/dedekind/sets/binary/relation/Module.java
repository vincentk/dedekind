package com.github.vincentk.dedekind.sets.binary.relation;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.Group;
import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.Ring;

/**
 * Definition of scalar multiplication over a {@link Ring}.
 *
 * Strictly speaking, this is the definition of a "right module"
 * defining multiplication by a scalar from the right.
 *
 * @see https://en.wikipedia.org/wiki/Module_(mathematics)
 */
public interface Module<
R extends Ring<R>,
C extends Cardinality,
N extends Module<R, C, N>>
extends
SemiModule<R, N>,
Group.P<N>
{
    @Override
    N mult(R scalar);

    @Override
    N plus(N module);
}
