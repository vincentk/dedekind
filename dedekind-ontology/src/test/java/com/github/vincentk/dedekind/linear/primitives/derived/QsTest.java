package com.github.vincentk.dedekind.linear.primitives.derived;

import static org.assertj.core.api.Assertions.assertThat;

import static com.github.vincentk.dedekind.linear.primitives.derived.Qs.*;

import org.junit.jupiter.api.Test;

public class QsTest {

	@Test
	public void testAddition() {

		assertThat(ZERO.plus(ZERO)).isEqualTo(Qs.of(0, 2));

		assertThat(ZERO.plus(UNIT)).isEqualTo(Qs.of(1, 1));
	}
}
