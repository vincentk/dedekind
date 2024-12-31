package com.github.vincentk.dedekind.algebra.structures;


import com.github.vincentk.dedekind.algebra.sets.Rings;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.binary.function.operation.Closure;

/**
 * A semiring: basically a ring but without the additive inverse.
 * 
 * @see https://en.wikipedia.org/wiki/Semiring
 */
public interface SemiRing<
S extends SemiRing.SmrE<S>,
C extends Cardinality,
R extends SemiRing<S, C, R>
>
extends
Rings,
Monoid.P<S, C, R>,
Monoid.M<S, C, R>
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
    C extends Cardinality.Countable,
    T extends Natural<E, C, T>>
    extends
    SemiRing<E, C, T>, Naturals,
    Closure<C, E, T, E, T, T>
    {}
}
