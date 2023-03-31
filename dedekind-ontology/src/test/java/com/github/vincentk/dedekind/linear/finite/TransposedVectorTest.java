/**
 * 
 */
package com.github.vincentk.dedekind.linear.finite;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.params.provider.Arguments.of;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.github.vincentk.dedekind.numbers.N;

public class TransposedVectorTest {

    @ParameterizedTest
    @MethodSource
    public void testTranspose(N b1) {

        final var v1 = One.of(b1);

        final var v1t = new TransposedRowVector<>(v1).transpose();

        assertThat(v1t).isEqualTo(v1);
    }

    private static Stream<Arguments> testTranspose() {
        return Stream.of(
                of(N.of(0)),
                of(N.of(1)),
                of(N.of(123))
                );
    }
}
