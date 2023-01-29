package com.github.vincentk.dedekind.linear.primitives;

import static com.github.vincentk.dedekind.linear.primitives.Doubles.doubles;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

public class DoublesTest {

    @Test
    public void testConstruction() {
        assertThat(doubles(null)).isEmpty();

        assertThat(doubles(new double[0])).isEmpty();

        final var subject = doubles(new double[] {0});
        assertNotNull(subject);

        assertTrue(subject.equals(subject));
    }

    @Test
    public void testVectorOps() {

        final Doubles<?> subject = doubles(new double[] {1, 2}).get();

        // Monoid identity operations:
        assertThat(subject.mult(Rs.ONE)).isEqualTo(subject);

        final var zeros = doubles(new double[] {0, 0}).get();

        assertThat(subject.plus(zeros)).isEqualTo(subject);
    }
}
