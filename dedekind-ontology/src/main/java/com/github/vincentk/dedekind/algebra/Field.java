package com.github.vincentk.dedekind.algebra;

/**
 * Marker type denoting a <a href="https://en.wikipedia.org/wiki/Field_(mathematics)">field</>.
 *
 * Sample type tags following the
 * <a href="https://en.wikipedia.org/wiki/Field_(mathematics)#Examples">examples on wikipedia</>
 * are provided below.
 */
public interface Field<F extends Field<F>> extends Ring<F> {

    /**
     * Complex numbers:
     */
    interface Complex<C extends Complex<C>> extends Field<C> {}

    /**
     * Real numbers:
     */
    interface Reals<R extends Reals<R>> extends Complex<R> {}
    
    /**
     * Rational numbers:
     */
    interface Rationals<Q extends Rationals<Q>> extends Reals<Q> {}


}
