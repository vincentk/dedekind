package com.github.vincentk.dedekind.numbers;

import static com.github.vincentk.dedekind.algebra.numbers.C.I1;
import static com.github.vincentk.dedekind.algebra.numbers.C.R1;
import static com.github.vincentk.dedekind.algebra.numbers.C.UNIT;
import static com.github.vincentk.dedekind.algebra.numbers.C.ZERO;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import static org.junit.jupiter.params.provider.Arguments.*;

import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.github.vincentk.dedekind.algebra.numbers.C;
import com.github.vincentk.dedekind.algebra.numbers.R;

public class CsTest {

	@Test
	public void testAddition() {
		assertEquals(ZERO, ZERO.十(ZERO));
		assertEquals(R1, R1.十(ZERO));
		assertEquals(R1, ZERO.十(R1));
		assertEquals(UNIT, R1.十(I1));
	}
	
	@Test
	public void testSubtraction() {
		assertEquals(ZERO, ZERO.一(ZERO));
		assertEquals(R1, R1.一(ZERO));
		assertEquals(R1.neg(), ZERO.一(R1));
		assertEquals(UNIT.conj(), R1.一(I1));
	}
	
	@Test
	public void testConjugate() {
		assertEquals(R1, R1.conj());
		assertEquals(I1.neg(), I1.conj());

		assertEquals(R1.十(I1.neg()), UNIT.conj());
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
		
		assertEquals(R1.neg().十(I1.neg()), UNIT.x(I1).x(I1));
	}
	
	/**
	 * N.b. : |C| -> R
	 */
	@ParameterizedTest
	@MethodSource
	public void testAbsolute(C.Ce complex, R.R64 expected, String name) {
	    assertThat(complex.abs2()).isEqualTo(expected);
	}
	
	private static Stream<Arguments> testAbsolute() {
	    return Stream.of(
	            of(ZERO, R.ZERO, "zero"),
	            of(R1, R.ONE, "real unit"),
	            of(I1, R.ONE, "imaginary unit"),
	            of(UNIT, R.TWO, "(1, 1)"),
	            of(UNIT.neg(), R.TWO, "(-1, -1)"),
	            of(UNIT.times(I1), R.TWO, "(1, -1)")
	            );
	}
	
}
