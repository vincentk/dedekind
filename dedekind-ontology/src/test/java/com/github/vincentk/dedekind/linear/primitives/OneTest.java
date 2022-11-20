package com.github.vincentk.dedekind.linear.primitives;

import static com.github.vincentk.dedekind.linear.primitives.Ints.ONE;
import static com.github.vincentk.dedekind.linear.primitives.Ints.THREE;
import static com.github.vincentk.dedekind.linear.primitives.Ints.TWO;
import static com.github.vincentk.dedekind.linear.primitives.Ints.ZERO;
import static com.github.vincentk.dedekind.linear.primitives.Ints.of;
import static com.github.vincentk.dedekind.linear.primitives.One.one;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.linear.primitives.Ints.Z;

public class OneTest {

	@Test
	public void testAddition() {
		
		checkPlus(ZERO, ZERO, ZERO);
		checkPlus(ONE, ONE, ZERO);
		checkPlus(ONE, ZERO, ONE);
		checkPlus(TWO, ONE, ONE);
		checkPlus(THREE, ONE, TWO);
		checkPlus(of(6), THREE, THREE);
	}
	
	private static void checkPlus(Z expected, Z a, Z b) {
		assertEquals(one(expected), one(a).plus(one(b)));
	}
	
	@Test
	public void testMultiplication() {
		checkTimes(ZERO, ZERO, ZERO);
		checkTimes(ZERO, ONE, ZERO);
		checkTimes(ZERO, ZERO, ONE);
		checkTimes(ONE, ONE, ONE);
		checkTimes(TWO, ONE, TWO);
		checkTimes(of(9), THREE, THREE);
	}
	
	private static void checkTimes(Z expected, Z a, Z b) {
		assertEquals(one(expected), one(a).mult(b));
	}
}
