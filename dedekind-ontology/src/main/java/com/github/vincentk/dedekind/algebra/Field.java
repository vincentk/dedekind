package com.github.vincentk.dedekind.algebra;

/**
 * Marker type denoting a <a href="https://en.wikipedia.org/wiki/Field_(mathematics)">field</>.
 *
 * Sample type tags following the
 * <a href="https://en.wikipedia.org/wiki/Field_(mathematics)#Examples">examples on wikipedia</>
 * are provided below.
 */
public interface Field extends Ring {

    /**
     * Rational numbers:
     */
    final class RATIONALS implements Field {}

    /**
     * Real numbers:
     */
    final class REALS implements Field {}

    /**
     * Complex numbers:
     */
    final class COMPLEX implements Field {}
}
