/**
 * 
 */
package com.github.vincentk.dedekind.algebra.numbers;

import com.github.vincentk.dedekind.algebra.numbers.N.Nat;
import com.github.vincentk.dedekind.algebra.sets.SemiRings.Booleans;
import com.github.vincentk.dedekind.algebra.structures.SemiRing;
import com.github.vincentk.dedekind.geometry.MetricSpace;
import com.github.vincentk.dedekind.geometry.NumberLine;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.FiniteSet;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered;

/**
 * Boolean values. Roughly speaking 0 <=> false, 1 <=> true.
 * 
 * @see https://en.wikipedia.org/wiki/Two-element_Boolean_algebra
 */
public interface B
extends
SemiRing<B.Bool, B>,
NumberLine<B.Bool, Cardinality.Finite, B>,
FiniteSet.B64<B.Bool, Cardinality.Finite.PowerOfTwo.B64, B>,
Booleans
{
    static Be bool(boolean n) {
	return n ? TRUE : FALSE;
    }

    public static final Be TRUE = new Be(true), FALSE = new Be(false);

    @Override
    default long cardinality() {
	return 2;
    }

    interface Bool
    extends
    SemiRing.SmrE<Bool>,
    TotallyOrdered.Oe<Bool>,
    MetricSpace.Me<Bool, Bool>
    {
	boolean bool();

	@Override
	default boolean isIdentityP() {
	    return !bool();
	}

	/**
	 * a && b ~ a * b
	 * 
	 * a && 1 = a => 1 is the monoid unit (~ multiplication).
	 * 
	 * @param that
	 * @return boolean product (and)
	 */
	default Bool and(Bool that) {
	    return x(that);
	}

	/**
	 * a || b ~ a + b
	 * 
	 * a || 0 = a => zero is the monoid unit (~ addition).
	 * 
	 * @param that
	 * @return boolean sum (or)
	 */
	default Bool or(Bool that) {
	    return plus(that);
	}

	@Override
	default Be plus(Bool that) {
	    return B.bool(bool() || that.bool());
	}

	default Be complement() {
	    return B.bool(!bool());
	}

	@Override
	default Be times(Bool that) {
	    return B.bool(bool() && that.bool());
	}

	@Override
	default int compareTo(Bool o) {
	    return Boolean.compare(bool(), o.bool());
	}

	@Override
	default Bool upperBound(Bool that) {
	    return plus(that);
	}

	@Override
	default Bool distance(Bool other) {
	    return B.bool(eq(other));
	}


	default Nat nat() {
	    return bool() ? N.ONE : N.ZERO;
	}
    }

    record Be (boolean bool) implements Bool {


    }
}
