package com.github.vincentk.dedekind.linear.primitives.derived;

import static com.github.vincentk.dedekind.linear.primitives.derived.Qs.UNIT;
import static com.github.vincentk.dedekind.linear.primitives.derived.Qs.ZERO;
import static com.github.vincentk.dedekind.linear.primitives.derived.Qs.of;
import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

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

	private static Qs q(int en, int de) {
		return of(en, de);
	}
}
