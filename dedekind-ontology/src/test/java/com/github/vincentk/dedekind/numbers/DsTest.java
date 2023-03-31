package com.github.vincentk.dedekind.numbers;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

public class DsTest {

    @Test
    public void testZeroEpsilon()
    {
        final var zero = Ds.of(0);
        final var one = Ds.of(1);
        final var two = Ds.of(2);

        assertThat(zero.x(zero)).isEqualTo(zero);
        assertThat(one.x(zero)).isEqualTo(zero);
        assertThat(zero.x(one)).isEqualTo(zero);

        assertThat(one.times(one)).isEqualTo(one);
        assertThat(one.p(zero)).isEqualTo(one);
        assertThat(zero.p(one)).isEqualTo(one);
        assertThat(one.p(one)).isEqualTo(two);
    }

    @Test
    public void testChainRule() {

        final double eps = 0.1;

        final var one = Ds.of(1, eps);

        assertThat(one.p(Ds.of(3))).isEqualTo(Ds.of(4, eps));

        assertThat(one.x(Ds.of(5))).isEqualTo(Ds.of(5, 5 * eps));

        assertThat(one.p(one)).isEqualTo(Ds.of(2, 2 * eps));
    }
}
