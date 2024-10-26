/**
 * 
 */
package com.github.vincentk.dedekind.algebra.numbers;

import com.github.vincentk.dedekind.algebra.sets.SemiRings.Booleans;
import com.github.vincentk.dedekind.algebra.structures.Group;
import com.github.vincentk.dedekind.algebra.structures.SemiRing;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Set;
import com.github.vincentk.dedekind.sets.ordered.TotallyOrdered;

/**
 * Boolean values. Roughly speaking 0 <=> false, 1 <=> true.
 * 
 * @see https://en.wikipedia.org/wiki/Two-element_Boolean_algebra
 */
public interface B
extends
SemiRing<B>,
Group.M0<B>,
Set.Finite<B>,
TotallyOrdered<Cardinality.Finite, B>,
Booleans
{

    public boolean bool();

    default long cardinality() {
	return 2;
    }

    /**
     * a && b ~ a * b
     * 
     * a && 1 = a => 1 is the monoid unit (~ multiplication).
     * 
     * @param that
     * @return boolean product (and)
     */
    default B and(B that) {
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
    default B or(B that) {
	return plus(that);
    }

    @Override
    default Be plus(B that) {
	return bool(bool() || that.bool());
    }

    @Override
    default Be inverse0() {
	return bool(!bool());
    }

    @Override
    default Be times(B that) {
	return bool(bool() && that.bool());
    }

    @Override
    default int compareTo(B o) {
	return Boolean.compare(bool(), o.bool());
    }

    default N nat() {
	return bool() ? N.ONE : N.ZERO;
    }

    static Be bool(boolean n) {
	return n ? TRUE : FALSE;
    }

    record Be (boolean bool) implements B {
    }

    public static final Be TRUE = new Be(true), FALSE = new Be(false);
}
