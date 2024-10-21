package com.github.vincentk.dedekind.linear.primitives;

import static com.github.vincentk.dedekind.algebra.numbers.Z.ONE;
import static com.github.vincentk.dedekind.algebra.numbers.Z.THREE;
import static com.github.vincentk.dedekind.algebra.numbers.Z.TWO;
import static com.github.vincentk.dedekind.algebra.numbers.Z.ZERO;
import static com.github.vincentk.dedekind.algebra.numbers.Z.integer;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

public class IntsTest {

	@Test
	public void testAddition() {
		assertEquals(ZERO, ZERO.plus(ZERO));
		assertEquals(ONE, ONE.plus(ZERO));
		assertEquals(ONE, ZERO.plus(ONE));
		assertEquals(TWO, ONE.plus(ONE));
		assertEquals(THREE, ONE.plus(TWO));
		assertEquals(integer(6), THREE.plus(THREE));
	}
	
	@Test
	public void testMultiplication() {
		assertEquals(ZERO, ZERO.times(ZERO));
		assertEquals(ZERO, ONE.times(ZERO));
		assertEquals(ZERO, ZERO.times(ONE));
		assertEquals(ONE, ONE.times(ONE));
		assertEquals(TWO, ONE.times(TWO));
		assertEquals(integer(9), THREE.times(THREE));
	}
}
