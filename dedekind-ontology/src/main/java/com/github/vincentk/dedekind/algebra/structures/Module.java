package com.github.vincentk.dedekind.algebra.structures;

/**
 * Definition of scalar multiplication over a {@link Ring}.
 *
 * Strictly speaking, this is the definition of a "right module"
 * defining multiplication by a scalar from the right.
 *
 * @see https://en.wikipedia.org/wiki/Module_(mathematics)
 */
public interface Module<
R extends Ring.Re<R>,
E extends Module.Me<R, E>,
N extends Module<R, E, N>>
extends
SemiModule<R, E, N>,
Group.P<E, N>
{
    interface Me<R extends Ring.SmrE<R>, E extends Me<R, E>>
    extends SemiModule.SmE<R, E>, Group.P.Pe<E>
    {	
    }
}
