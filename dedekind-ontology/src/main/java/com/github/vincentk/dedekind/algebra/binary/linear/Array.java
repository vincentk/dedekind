/**
 * 
 */
package com.github.vincentk.dedekind.algebra.binary.linear;

import java.util.Optional;

import com.github.vincentk.dedekind.algebra.binary.Dual;
import com.github.vincentk.dedekind.algebra.binary.Module;
import com.github.vincentk.dedekind.algebra.unary.Ring;
import com.github.vincentk.dedekind.sets.AoC;
import com.github.vincentk.dedekind.sets.AoC.Enumeration;
import com.github.vincentk.dedekind.sets.Cardinality;

public interface Array<
F extends Ring<F>,
C extends Cardinality.Countable,
S extends Array<F, C, S>
>
extends
Module<F, C, S>,
AoC<F, Enumeration<F>>,
Dual<S, S>
{
    Optional<S> fromEnumeration(Enumeration<F> seq);
    
    @Override
    default S mult(F scalar) {
        
        final Enumeration<F> scaled = () -> enumeration().next().map(x -> x.times(scalar));
        
        return fromEnumeration(scaled).get();
    }

    @Override
    default S plus(S that) {
        
        final Enumeration<F> e1 = enumeration(), other = that.enumeration();
        
        final Enumeration<F> sum = () -> e1.next().flatMap(x -> other.next().map(y -> y.plus(x)));
        
        return fromEnumeration(sum).get();
    }

    @Override
    default S negate() {
        
        final Enumeration<F> negated = () -> enumeration().next().map(Ring::negate);
        
        return fromEnumeration(negated).get();
    }
    
    @Override
    default S transpose() {
        return fromEnumeration(enumeration()).get();
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
