package com.github.vincentk.dedekind.linear.primitives.derived;

import static com.github.vincentk.dedekind.linear.primitives.derived.Cs.ZERO;

import static com.github.vincentk.dedekind.linear.primitives.derived.Cs.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

public class CsTest {

	@Test
	public void testAddition() {
		assertEquals(ZERO, ZERO.p(ZERO));
		assertEquals(R1, R1.p(ZERO));
		assertEquals(R1, ZERO.p(R1));
		assertEquals(UNIT, R1.p(I1));
	}
	
	@Test
	public void testSubtraction() {
		assertEquals(ZERO, ZERO.minus(ZERO));
		assertEquals(R1, R1.minus(ZERO));
		assertEquals(R1.neg(), ZERO.minus(R1));
		assertEquals(UNIT.conj(), R1.minus(I1));
	}
	
	@Test
	public void testConjugate() {
		assertEquals(R1, R1.conj());
		assertEquals(I1.neg(), I1.conj());

		assertEquals(R1.p(I1.neg()), UNIT.conj());
	}
	
	@Test
	public void testMultiplication() {
		assertEquals(ZERO, ZERO.x(ZERO));
		assertEquals(ZERO, R1.x(ZERO));
		assertEquals(ZERO, ZERO.x(R1));
		
		assertEquals(R1, R1.x(R1));
		assertEquals(R1.neg(), I1.x(I1));
		assertEquals(I1, R1.x(I1));
		
		assertEquals(R1.neg(), I1.x(I1));
		assertEquals(I1.neg(), I1.x(I1).x(I1));
		assertEquals(R1, I1.x(I1).x(I1).x(I1));
		
		assertEquals(R1.x(R1), I1.x(I1.conj()));
		
		assertEquals(I1, R1.x(I1).x(R1));
		assertEquals(I1, R1.x(R1).x(I1));
		
		assertEquals(R1.neg().p(I1.neg()), UNIT.x(I1).x(I1));
	}
}
