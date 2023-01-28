package com.github.vincentk.dedekind.linear.primitives;

import java.util.Arrays;

import com.github.vincentk.dedekind.algebra.Equality;
import com.github.vincentk.dedekind.linear.Vector;

/**
 * Vector with just one element.
 * 
 * @param <R> type of ring defining the element type.
 */
final class Doubles
implements
Vector<Rs, Doubles>,
Equality<Doubles>
{
	private final double[] val;

	private Doubles(double[] val) {
		
		assert null != val;
		
		this.val = val;
	}

	@Override
	public Doubles mult(Rs scalar) {

		final var na = Arrays.copyOf(val, val.length);

		final double sv = scalar.doubleVal();

		for (int ii = 0; ii < val.length; ii++) {
			na[ii] *= sv;
		}

		return new Doubles(na);
	}

	@Override
	public Doubles plus(Doubles vector) {

		assert val.length == vector.val.length;

		final var na = Arrays.copyOf(val, val.length);

		final var va = vector.val;
		for (int ii = 0; ii < val.length; ii++) {
			na[ii] += va[ii];
		}

		return new Doubles(na);
	}

	@Override
	public boolean equals(Doubles that) {
		return Arrays.equals(this.val, that.val);
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof Doubles) {
			final Doubles that = (Doubles) other;
			if (this.val.getClass().isAssignableFrom(that.val.getClass())) {
				return equals((Doubles) other);
			}
		}

		return false;
	}
	
	@Override
	public String toString() {
		return Arrays.toString(val);
	}

	public static
	Doubles
	doubles(double[] val) {
		return new Doubles(val);
	}
}