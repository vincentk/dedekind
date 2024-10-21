package com.github.vincentk.dedekind.numbers;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.params.provider.Arguments.of;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.github.vincentk.dedekind.algebra.numbers.B;

public class BeTest {

    @ParameterizedTest
    @MethodSource
    public void testAddition(B b1, B b2) {
        assertThat(b1.plus(b2)).isEqualTo(b1.or(b2));
    }

    private static Stream<Arguments> testAddition() {
        return Stream.of(
                of(B.bool(true), B.bool(true)),
                of(B.bool(false), B.bool(false)),
                of(B.bool(true), B.bool(false)),
                of(B.bool(false), B.bool(true))
                );
    }
    
    @ParameterizedTest
    @MethodSource
    public void testMultiplication(B b1, B b2) {
        assertThat(b1.times(b2)).isEqualTo(b1.and(b2));
    }

    private static Stream<Arguments> testMultiplication() {
        return Stream.of(
                of(B.bool(true), B.bool(true)),
                of(B.bool(false), B.bool(false)),
                of(B.bool(true), B.bool(false)),
                of(B.bool(false), B.bool(true))
                );
    }
}
