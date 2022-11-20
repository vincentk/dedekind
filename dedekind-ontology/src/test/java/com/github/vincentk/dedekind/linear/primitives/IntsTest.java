package com.github.vincentk.dedekind.linear.primitives;

import static com.github.vincentk.dedekind.linear.primitives.Ints.ONE;
import static com.github.vincentk.dedekind.linear.primitives.Ints.THREE;
import static com.github.vincentk.dedekind.linear.primitives.Ints.TWO;
import static com.github.vincentk.dedekind.linear.primitives.Ints.ZERO;
import static com.github.vincentk.dedekind.linear.primitives.Ints.of;
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
		assertEquals(of(6), THREE.plus(THREE));
	}
	
	@Test
	public void testMultiplication() {
		assertEquals(ZERO, ZERO.times(ZERO));
		assertEquals(ZERO, ONE.times(ZERO));
		assertEquals(ZERO, ZERO.times(ONE));
		assertEquals(ONE, ONE.times(ONE));
		assertEquals(TWO, ONE.times(TWO));
		assertEquals(of(9), THREE.times(THREE));
	}
}
