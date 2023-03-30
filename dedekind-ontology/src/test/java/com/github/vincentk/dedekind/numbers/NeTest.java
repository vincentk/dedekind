package com.github.vincentk.dedekind.numbers;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.params.provider.Arguments.of;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

public class NeTest {

    @ParameterizedTest
    @MethodSource
    public void testAddition(N b1, N b2, N expected, String name) {
        assertThat(b1.p(b2)).isEqualTo(expected);
    }

    private static Stream<Arguments> testAddition() {
        return Stream.of(
                of(N.of(0), N.of(1), N.of(1), "1 + 1 == 1"),
                of(N.of(0), N.of(0), N.of(0), "0 + 0 == 0"),
                of(N.of(1), N.of(0), N.of(1), "1 + 0 == 1"),
                of(N.of(0), N.of(1), N.of(1), "0 + 1 == 1")
                );
    }
    
    @ParameterizedTest
    @MethodSource
    public void testMultiplication(N b1, N b2, N expected, String name) {
        assertThat(b1.times(b2)).isEqualTo(expected);
    }

    private static Stream<Arguments> testMultiplication() {
        return Stream.of(
                of(N.of(1), N.of(1), N.of(1), "1 * 1 == 1"),
                of(N.of(0), N.of(0), N.of(0), "0 * 0 == 0"),
                of(N.of(1), N.of(0), N.of(0), "1 * 0 == 0"),
                of(N.of(0), N.of(1), N.of(0), "0 * 1 == 0")
                );
    }
    
    @ParameterizedTest
    @MethodSource
    public void testDistance(N b1, N b2, N expected) {
        assertThat(b1.distance(b2)).isEqualTo(expected);
        assertThat(b2.distance(b1)).isEqualTo(expected);
    }

    private static Stream<Arguments> testDistance() {
        return Stream.of(
                of(N.of(1), N.of(1), N.of(0)),
                of(N.of(0), N.of(0), N.of(0)),
                of(N.of(1), N.of(0), N.of(1)),
                of(N.of(0), N.of(1), N.of(1))
                );
    }
}
