package com.github.vincentk.dedekind.algebra.sets;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.sets.Fields.Complex;
import com.github.vincentk.dedekind.algebra.sets.Fields.Rationals;
import com.github.vincentk.dedekind.algebra.sets.Fields.Reals;
import com.github.vincentk.dedekind.algebra.sets.Rings.Integers;
import com.github.vincentk.dedekind.algebra.sets.SemiRings.Naturals;

public class FieldsTest {
	
	/**
	 * Tests that subset operation is available through sub-typing
	 * by way of spelling out an explicit type check.
	 */
	@Test
	public void testSubsetRelationship() {
		final Naturals nat = null;
		final Integers nit = nat;
		final Rationals rat = nit;
		final Reals real = rat;
		@SuppressWarnings("unused")
		final Complex cpx = real;
	}
}
