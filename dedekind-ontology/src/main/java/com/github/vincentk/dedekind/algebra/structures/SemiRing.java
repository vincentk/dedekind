package com.github.vincentk.dedekind.algebra.structures;


import com.github.vincentk.dedekind.algebra.sets.Rings;
import com.github.vincentk.dedekind.sets.binary.function.operation.Closure;

/**
 * A semiring: basically a ring but without the additive inverse.
 * 
 * @see https://en.wikipedia.org/wiki/Semiring
 */
public interface SemiRing<
S extends SemiRing.SmrE<S>,
R extends SemiRing<S, R>
>
extends
Rings,
Monoid.P<S, R>,
Monoid.M<S, R>
{
    interface SmrE<E extends SmrE<E>>
    extends
    Monoid.P.Pe<E>,
    Monoid.M.Te<E>
    {
	@Override
	default E ap(E that) {
	    return Pe.super.ap(that);
	}

	@Override
	default boolean isIdentity() {
	    return Pe.super.isIdentity();
	}

	@Override
	@SuppressWarnings("unchecked")
	default boolean isIdentityM() {
	    // x * x = x
	    // =>
	    // x = 0
	    // or
	    // x = 1
	    // i.e.
	    // 1 * 1 = 1
	    return !isIdentityP() && eq(times((E) this));
	}	
    }

    /**
     * The natural numbers are a well-known semi-ring.
     * They are the closure of the integers under addition and multiplication.
     * @param <N>
     */
    interface Natural<
    E extends SmrE<E>,
    T extends Natural<E, T>>
    extends
    SemiRing<E, T>, Naturals,
    Closure<E, T, E, T, T>
    {}
}
