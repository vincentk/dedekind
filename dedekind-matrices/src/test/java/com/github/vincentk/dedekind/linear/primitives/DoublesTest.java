package com.github.vincentk.dedekind.linear.primitives;

import static com.github.vincentk.dedekind.linear.primitives.Doubles.doubles;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.linear.Vector;

public class DoublesTest {

    @Test
    public void testConstruction() {
        assertThrows(AssertionError.class, () -> doubles(null));

        assertThrows(AssertionError.class, () -> doubles(new double[0]));

        final var subject = doubles(new double[] {0});
        assertNotNull(subject);

        assertTrue(subject.equals(subject));
    }

    @Test
    public void testVectorOps() {

        final Vector<Rs, Doubles> subject = doubles(new double[] {1, 2});

        // Monoid identity operations:
        assertThat(subject.mult(Rs.ONE)).isEqualTo(subject);

        final var zeros = doubles(new double[] {0, 0});

        assertThat(subject.plus(zeros)).isEqualTo(subject);
    }
}
