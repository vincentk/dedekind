/**
 * 
 */
package com.github.vincentk.dedekind.algebra.binary.linear;

import java.util.Optional;
import java.util.function.BinaryOperator;

import com.github.vincentk.dedekind.algebra.binary.Dual;
import com.github.vincentk.dedekind.algebra.binary.Module;
import com.github.vincentk.dedekind.algebra.unary.Ring;
import com.github.vincentk.dedekind.sets.AoC;
import com.github.vincentk.dedekind.sets.AoC.Enumeration;
import com.github.vincentk.dedekind.sets.AoC.Enumeration.Pair;
import com.github.vincentk.dedekind.sets.Cardinality;

public interface Array<
F extends Ring<F>,
C extends Cardinality.Countable,
S extends Array<F, C, S>
>
extends
Module<F, C, S>,
AoC<F, Enumeration<F>>,
Dual<S, S>,
InnerProduct<F, S>
{
    Optional<S> fromEnumeration(Enumeration<F> seq);

    @Override
    default S mult(F scalar) {
        return fromEnumeration(enumeration().map(x -> x.times(scalar))).get();
    }

    @Override
    default S plus(S that) {

        final Enumeration<F> ps = enumeration().zip(that.enumeration()).map(p -> p.a().plus(p.b()));

        return fromEnumeration(ps).get();
    }

    @Override
    default S negate() {
        return fromEnumeration(enumeration().map(Ring::negate)).get();
    }

    @Override
    default S transpose() {
        return fromEnumeration(enumeration()).get();
    }

    @Override
    default F dot(S that) {

        final Enumeration<Pair<F, F>> ps = enumeration().zip(that.enumeration());

        final Enumeration<F> ps1 = ps.map(p -> p.a().times(p.b()));

        return ps1.fold((x, y) -> x.times(y));
    }

    @Override
    default S zero() {
        return fromEnumeration(enumeration().map(x -> x.zero())).get();
    }

    interface Finite
    <
    F extends Ring<F>,
    C extends Cardinality.Finite
    >
    extends Array<F, C, Finite<F, C>>
    {}

    record OneOff<
    F extends Ring<F>,
    C extends Cardinality.Countable
    >
    (Enumeration<F> enumeration)
    implements Array<F, C, OneOff<F, C>>
    {
        @Override
        public Optional<OneOff<F, C>> fromEnumeration(Enumeration<F> seq) {
            return Optional.of(new OneOff<>(seq));
        }}
}
