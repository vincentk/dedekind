package com.github.vincentk.dedekind.numbers;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.params.provider.Arguments.of;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

public class BeTest {

    @ParameterizedTest
    @MethodSource
    public void testAddition(B b1, B b2, B expected, String name) {
        assertThat(b1.plus(b2)).isEqualTo(expected);
    }

    private static Stream<Arguments> testAddition() {
        return Stream.of(
                of(B.of(true), B.of(true), B.of(true), "1 || 1 == 1"),
                of(B.of(false), B.of(false), B.of(false), "0 || 0 == 0"),
                of(B.of(true), B.of(false), B.of(true), "1 || 0 == 1"),
                of(B.of(false), B.of(true), B.of(true), "0 || 1 == 1")
                );
    }
}
