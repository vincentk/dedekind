package com.github.vincentk.dedekind.algebra.peano;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

public class PeanoTest {
	
	@Test
	public void testAddition() {
		assertEquals(0, Peano.ZERO.plus(Peano.ZERO).longVal());
		
		assertEquals(1, Peano.ONE.plus(Peano.ZERO).longVal());
		
		assertEquals(1, Peano.ZERO.plus(Peano.ONE).longVal());
		
		assertEquals(2, Peano.ONE.plus(Peano.ONE).longVal());
	}

}
