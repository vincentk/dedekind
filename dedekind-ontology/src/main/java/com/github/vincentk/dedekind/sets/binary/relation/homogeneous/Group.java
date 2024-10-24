package com.github.vincentk.dedekind.sets.binary.relation.homogeneous;

import java.util.Optional;

/**
 * A {@link Monoid} with the following additional properties:
 * 
 * a) existence of an inverse operation such that x * x' = 1.
 *
 * @see https://en.wikipedia.org/wiki/Group_(mathematics)
 * 
 * @param <T> the implementation type
 */
public interface Group<T extends Group<T>> extends Monoid<T> {

    /**
     * Group under addition (M, +, -).
     * 
     * @param <T> the implementing type.
     */
    interface P<T extends P<T>> extends Group<T>, Monoid.P<T> {

        /**
         * @return - this
         */
        T negate();

        default T neg() {
            return negate();
        }

        default T minus(T that) {
            return plus(that.neg());
        }
    }

    /**
     * Group under multiplication (M, *, /).
     * 
     * Notably, inversion is a partial function.
     * 
     * Division by zero and similar are typically undefined.
     * 
     * @param <T> the implementing type.
     */
    interface M<T extends M<T>> extends Group<T>, Monoid.M<T> {

        Optional<T> inverse();

        default Optional<T> inv() {
            return inverse();
        }

        default Optional<T> divide(T that) {
            return that.inv().map(this::times);
        }
    }

    /**
     * Group under multiplication (M, *, /).
     * 
     * The implementor guarantees at or before construction time that
     * inversion is a total function (e.g. by ensuring that a zero element
     * can not be constructed).
     * 
     * E.g. we are looking at "the reals without zero", 
     * "the set of invertible matrices" or similar.
     * 
     * @param <T> the implementing type.
     */
    interface M0<T extends M0<T>> extends M<T> {

        T inverse0();

        default T inv0() {
            return inverse0();
        }

        default T divide0(T that) {
            return times(that.inv0());
        }

        @Override
        default Optional<T> inverse() {
            return Optional.of(inverse0());
        }

        @Override
        default Optional<T> divide(T that) {
            return Optional.of(divide0(that));
        }
    }
}
