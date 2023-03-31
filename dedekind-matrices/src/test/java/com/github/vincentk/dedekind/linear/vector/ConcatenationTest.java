/**
 * 
 */
package com.github.vincentk.dedekind.linear.vector;

import static com.github.vincentk.dedekind.linear.finite.One.one;
import static com.github.vincentk.dedekind.numbers.N.nat;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.params.provider.Arguments.of;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.github.vincentk.dedekind.linear.finite.One;
import com.github.vincentk.dedekind.linear.RowVector;
import com.github.vincentk.dedekind.numbers.N;

public class ConcatenationTest {

    @ParameterizedTest
    @MethodSource
    public void testConcat1(N b1) {

        final One<N> v1 = one(b1);

        final var tst = Concatenation.finite(v1, v1);
        
        final RowVector<N, ?, ?, ?> sum = tst.plus(tst);

        assertThat(sum).isNotNull();
        
        final N dt = tst.dot(tst.transpose());
        
        // [n, n]^2 = 2 * n^2
        final N twoN2 = nat(2).times(b1.abs2());
        assertThat(dt).isEqualTo(twoN2);
    }

    private static Stream<Arguments> testConcat1() {
        return Stream.of(
                of(nat(0)),
                of(nat(1)),
                of(nat(123))
                );
    }
}
