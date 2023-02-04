package com.github.vincentk.dedekind.linear.finite;

import static com.github.vincentk.dedekind.numbers.Z.ONE;
import static com.github.vincentk.dedekind.numbers.Z.THREE;
import static com.github.vincentk.dedekind.numbers.Z.TWO;
import static com.github.vincentk.dedekind.numbers.Z.ZERO;
import static com.github.vincentk.dedekind.numbers.Z.of;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.linear.LinearMap;
import com.github.vincentk.dedekind.numbers.Z;

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
        assertEquals(One.of(expected), One.of(a).plus(One.of(b)));
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

    @Test
    public void outerProductTest() {

        final var zero = One.of(ZERO);

        assertThat(zero.outer(zero)).isInstanceOf(LinearMap.class);

        assertThat(zero.outer(zero).apply(zero)).isEqualTo(zero);

        final var one = One.of(ONE);
        
        assertThat(zero.outer(one).apply(zero)).isEqualTo(zero);

        assertThat(one.outer(one).apply(one)).isEqualTo(one);
        
        final var two = One.of(TWO);
        
        assertThat(two.outer(one).apply(one)).isEqualTo(two);

        final var four = two.plus(two);
        
        assertThat(two.outer(two).apply(one)).isEqualTo(four);
        
        assertThat(two.outer(two).apply(two)).isEqualTo(four.plus(four));
    }

    private static void checkTimes(Z expected, Z a, Z b) {
        assertEquals(One.of(expected), One.of(a).mult(b));
    }
}
