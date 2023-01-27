package com.github.vincentk.dedekind.sets;

/**
 * Marker type denoting a <a href="https://en.wikipedia.org/wiki/Field_(mathematics)">field</>.
 *
 * Sample type tags following the
 * <a href="https://en.wikipedia.org/wiki/Field_(mathematics)#Examples">examples on wikipedia</>
 * are provided below.
 */
public interface Fields extends Rings {

    /**
     * The complex numbers form a field.
     */
    interface Complex extends Fields {}

    /**
     * The real numbers are a subset of the complex numbers.
     */
    interface Reals extends Complex {}
    
    /**
     * The rational numbers are a subset of the real numbers.
     */
    interface Rationals extends Reals {}
}
