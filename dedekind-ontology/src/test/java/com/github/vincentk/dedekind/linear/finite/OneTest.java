package com.github.vincentk.dedekind.linear.finite;

import static com.github.vincentk.dedekind.linear.finite.One.of;
import static com.github.vincentk.dedekind.linear.primitives.Zs.ONE;
import static com.github.vincentk.dedekind.linear.primitives.Zs.THREE;
import static com.github.vincentk.dedekind.linear.primitives.Zs.TWO;
import static com.github.vincentk.dedekind.linear.primitives.Zs.ZERO;
import static com.github.vincentk.dedekind.linear.primitives.Zs.of;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.linear.primitives.Zs;

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
	
	private static void checkPlus(Zs expected, Zs a, Zs b) {
		assertEquals(of(expected), of(a).plus(of(b)));
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
	
	private static void checkTimes(Zs expected, Zs a, Zs b) {
		assertEquals(of(expected), of(a).mult(b));
	}
}
