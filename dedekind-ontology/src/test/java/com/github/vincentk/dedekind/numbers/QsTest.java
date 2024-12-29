package com.github.vincentk.dedekind.numbers;

import static com.github.vincentk.dedekind.algebra.numbers.Q.UNIT;
import static com.github.vincentk.dedekind.algebra.numbers.Q.ZERO;
import static com.github.vincentk.dedekind.algebra.numbers.Q.rational;
import static org.assertj.core.api.Assertions.assertThat;

import java.util.Optional;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.Q;

public class QsTest {

	@Test
	public void testConstructor() {

		assertThat(q(2, 4)).isEqualTo(q(1, 2));

		assertThat(q(4, 2)).isEqualTo(q(2, 1));

		assertThat(q(2, 6)).isEqualTo(q(1, 3));

		assertThat(q(3, 6)).isEqualTo(q(1, 2));
		
		assertThat(q(4, 6)).isEqualTo(q(2, 3));
		
		assertThat(q(27, 12)).isEqualTo(q(9, 4));
	}

	@Test
	public void testAddition() {

		assertThat(ZERO.plus(ZERO)).isEqualTo(q(0, 1));

		assertThat(ZERO.plus(UNIT)).isEqualTo(q(1, 1));

		assertThat(UNIT.plus(UNIT)).isEqualTo(q(2, 1));

		assertThat(q(1, 2).plus(q(1, 3))).isEqualTo(q(5, 6));
	}
	
	@Test
	public void testMultiplication() {

		assertThat(ZERO.x(ZERO)).isEqualTo(q(0, 1));

		assertThat(ZERO.x(UNIT)).isEqualTo(q(0, 1));

		assertThat(UNIT.x(UNIT)).isEqualTo(q(1, 1));

		assertThat(q(1, 2).x(q(1, 3))).isEqualTo(q(1, 6));
	}
	
	@Test
	public void testInverse() {
		assertThat(UNIT.inverse()).isEqualTo(Optional.of(UNIT));
	}
	
	@Test
	public void testSimplify() {
		assertThat(q(0, 300)).isEqualTo(ZERO);
		assertThat(q(300, 300)).isEqualTo(UNIT);
		assertThat(q(2, 4)).isEqualTo(q(1, 2));
	}
	
	@Test
	public void testCompare() {
	    assertThat(ZERO.compareTo(ZERO)).isEqualTo(0);
	    assertThat(UNIT.compareTo(UNIT)).isEqualTo(0);
	    assertThat(ZERO.compareTo(UNIT)).isEqualTo(-1);
	    assertThat(UNIT.compareTo(ZERO)).isEqualTo(1);
	    
	    assertThat(q(1, -2).de().intValue()).isEqualTo(2);
	    assertThat(q(1, -2).compareTo(ZERO)).isEqualTo(-1);
	    assertThat(q(1, -2).compareTo(q(2, -4))).isEqualTo(0);
	}
	
	@Test
	public void testMetric() {
	    assertThat(ZERO.distance(ZERO)).isEqualTo(ZERO);
	    assertThat(ZERO.distance(UNIT)).isEqualTo(UNIT);
	    assertThat(UNIT.distance(ZERO)).isEqualTo(UNIT);
	    assertThat(UNIT.neg().distance(ZERO)).isEqualTo(UNIT);
	}

	private static Q.Rat q(int en, int de) {
		return rational(en, de);
	}
}
