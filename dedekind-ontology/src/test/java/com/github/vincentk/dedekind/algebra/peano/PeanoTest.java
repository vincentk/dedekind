package com.github.vincentk.dedekind.algebra.peano;

import static com.github.vincentk.dedekind.algebra.peano.Peano.ONE;
import static com.github.vincentk.dedekind.algebra.peano.Peano.THREE;
import static com.github.vincentk.dedekind.algebra.peano.Peano.TWO;
import static com.github.vincentk.dedekind.algebra.peano.Peano.ZERO;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.function.BinaryOperator;

import org.junit.jupiter.api.Test;

public class PeanoTest {
	
	@Test
	public void testAddition() {
		plus(0, ZERO, ZERO);
		plus(1, ZERO, ONE);
		plus(2, ZERO, TWO);
		plus(3, ZERO, THREE);
		plus(2, ONE, ONE);
		plus(3, ONE, TWO);
		plus(4, ONE, THREE);
		plus(4, TWO, TWO);
		plus(5, TWO, THREE);
		plus(6, THREE, THREE);
	}
	
	private static void plus(
			long expected, Peano<?> a, Peano<?> b) {
		
		sym(expected, a, b, (x, y) -> x.plus(y));
	}

	@Test
	public void testMultiplication() {
		times(0, ZERO, ZERO);
		times(0, ZERO, ONE);
		times(0, ZERO, THREE);
		times(1, ONE, ONE);
		times(2, ONE, TWO);
		times(3, ONE, THREE);
		times(4, TWO, TWO);
		times(6, TWO, THREE);
		times(9, THREE, THREE);
	}
	
	private static void times(
			long expected, Peano<?> a, Peano<?> b) {
		
		sym(expected, a, b, (x, y) -> x.times(y));
	}
	
	private static void sym(
			long expected, Peano<?> a, Peano<?> b,
			BinaryOperator<Peano<?>> op) {
		
		equals(expected, op.apply(a, b));
		equals(expected, op.apply(b, a));
	}	
	
	
	private static void equals(long expected, Peano<?> found) {
		assertEquals(expected, found.longVal());
	}
}
