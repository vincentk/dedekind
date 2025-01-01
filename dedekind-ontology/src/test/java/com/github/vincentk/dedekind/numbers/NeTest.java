package com.github.vincentk.dedekind.numbers;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.params.provider.Arguments.of;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.github.vincentk.dedekind.algebra.numbers.N;
import com.github.vincentk.dedekind.algebra.numbers.N.N63.Ne;

public class NeTest {

    @ParameterizedTest
    @MethodSource
    public void testAddition(Ne b1, Ne b2, Ne expected, String name) {
        assertThat(b1.十(b2)).isEqualTo(expected);
    }

    private static Stream<Arguments> testAddition() {
        return Stream.of(
                of(N.nat(0), N.nat(1), N.nat(1), "1 + 1 == 1"),
                of(N.nat(0), N.nat(0), N.nat(0), "0 + 0 == 0"),
                of(N.nat(1), N.nat(0), N.nat(1), "1 + 0 == 1"),
                of(N.nat(0), N.nat(1), N.nat(1), "0 + 1 == 1")
                );
    }
    
    @ParameterizedTest
    @MethodSource
    public void testMultiplication(Ne b1, Ne b2, Ne expected, String name) {
        assertThat(b1.times(b2)).isEqualTo(expected);
    }

    private static Stream<Arguments> testMultiplication() {
        return Stream.of(
                of(N.nat(1), N.nat(1), N.nat(1), "1 * 1 == 1"),
                of(N.nat(0), N.nat(0), N.nat(0), "0 * 0 == 0"),
                of(N.nat(1), N.nat(0), N.nat(0), "1 * 0 == 0"),
                of(N.nat(0), N.nat(1), N.nat(0), "0 * 1 == 0")
                );
    }
    
    @ParameterizedTest
    @MethodSource
    public void testDistance(Ne b1, Ne b2, Ne expected) {
        assertThat(b1.distance(b2)).isEqualTo(expected);
        assertThat(b2.distance(b1)).isEqualTo(expected);
    }

    private static Stream<Arguments> testDistance() {
        return Stream.of(
                of(N.nat(1), N.nat(1), N.nat(0)),
                of(N.nat(0), N.nat(0), N.nat(0)),
                of(N.nat(1), N.nat(0), N.nat(1)),
                of(N.nat(0), N.nat(1), N.nat(1))
                );
    }
}
