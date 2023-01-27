package com.github.vincentk.dedekind.linear.primitives;

import static com.github.vincentk.dedekind.linear.primitives.Zs.ONE;
import static com.github.vincentk.dedekind.linear.primitives.Zs.THREE;
import static com.github.vincentk.dedekind.linear.primitives.Zs.TWO;
import static com.github.vincentk.dedekind.linear.primitives.Zs.ZERO;
import static com.github.vincentk.dedekind.linear.primitives.Zs.of;
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
