package com.github.vincentk.dedekind.linear.finite;

import static com.github.vincentk.dedekind.algebra.numbers.Z.ONE;
import static com.github.vincentk.dedekind.algebra.numbers.Z.THREE;
import static com.github.vincentk.dedekind.algebra.numbers.Z.TWO;
import static com.github.vincentk.dedekind.algebra.numbers.Z.ZERO;
import static com.github.vincentk.dedekind.algebra.numbers.Z.integer;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.Z;
import com.github.vincentk.dedekind.linear.LinearMap;

public class OneTest {

    @Test
    public void testAddition() {

        checkPlus(ZERO, ZERO, ZERO);
        checkPlus(ONE, ONE, ZERO);
        checkPlus(ONE, ZERO, ONE);
        checkPlus(TWO, ONE, ONE);
        checkPlus(THREE, ONE, TWO);
        checkPlus(integer(6), THREE, THREE);
    }

    private static void checkPlus(Z expected, Z a, Z b) {
        assertEquals(One.oneOf(expected), One.oneOf(a).plus(One.oneOf(b)));
    }

    @Test
    public void testMultiplication() {
        checkTimes(ZERO, ZERO, ZERO);
        checkTimes(ZERO, ONE, ZERO);
        checkTimes(ZERO, ZERO, ONE);
        checkTimes(ONE, ONE, ONE);
        checkTimes(TWO, ONE, TWO);
        checkTimes(integer(9), THREE, THREE);
    }

    @Test
    public void testOuterProduct() {

        final var zero = One.oneOf(ZERO);

        final var zerot = zero.transpose();

        assertThat(zerot.outer(zero)).isInstanceOf(LinearMap.class);

        assertThat(zerot.outer(zero).apply(zerot).transpose()).isEqualTo(zero);

        final var one = One.oneOf(ONE);
        final var onet = one.transpose();
        
        assertThat(zerot.outer(one).apply(zerot).transpose()).isEqualTo(zero);

        assertThat(onet.outer(one).apply(onet).transpose()).isEqualTo(one);

        final var two = One.oneOf(TWO);
        final var twot = two.transpose();

        assertThat(twot.outer(one).apply(onet).transpose()).isEqualTo(two);

        final var four = two.plus(two);

        assertThat(twot.outer(two).apply(onet).transpose()).isEqualTo(four);

        assertThat(twot.outer(two).apply(twot).transpose()).isEqualTo(four.plus(four));
    }

    private static void checkTimes(Z expected, Z a, Z b) {
        assertEquals(One.oneOf(expected), One.oneOf(a).mult(b));
    }
}
