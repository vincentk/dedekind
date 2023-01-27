package com.github.vincentk.dedekind.algebra;

import com.github.vincentk.dedekind.sets.Fields;

/**
 * Marker type denoting a <a href="https://en.wikipedia.org/wiki/Field_(mathematics)">field</>.
 *
 * Sample type tags following the
 * <a href="https://en.wikipedia.org/wiki/Field_(mathematics)#Examples">examples on wikipedia</>
 * are provided below.
 */
public interface Field<F extends Field<F>> extends Ring<F> {
	
	/**
	 * Subtraction (inverse of addition).
	 * 
	 * @param that
	 * @return this - that
	 */
	default F minus(F that) {
		return plus(that.negate());
	}
	
	/**
	 * @return 1 / this
	 */
	F inverse();
	
	default F inv() {
		return inverse();
	}
	
	/**
	 * Division (inverse of addition).
	 * 
	 * @param that
	 * @return this / that
	 */
	default F divide(F that) {
		return times(that.inverse());
	}
	
	default F div(F that) {
		return divide(that);
	}

    /**
     * Complex numbers:
     */
    interface Complex<C extends Complex<C>> extends Field<C> {
    	
    	/**
    	 * @return the complex conjugate
    	 */
    	C conjugate();
    	
    	default C conj() {
    		return conjugate();
    	}
    }

    /**
     * Real numbers:
     */
    interface Reals<R extends Reals<R>> extends Field<R>, Fields.Reals {}
    
    /**
     * Rational numbers:
     */
    interface Rationals<Q extends Rationals<Q>> extends Field<Q>, Fields.Rationals {}


}
