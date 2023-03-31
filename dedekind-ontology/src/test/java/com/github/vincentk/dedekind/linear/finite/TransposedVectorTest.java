/**
 * 
 */
package com.github.vincentk.dedekind.linear.finite;

import static com.github.vincentk.dedekind.linear.finite.One.one;
import static com.github.vincentk.dedekind.linear.finite.TransposedRowVector.transposed;
import static com.github.vincentk.dedekind.numbers.N.nat;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.params.provider.Arguments.of;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.github.vincentk.dedekind.linear.OuterProductSpace.Bra;
import com.github.vincentk.dedekind.linear.OuterProductSpace.Ket;
import com.github.vincentk.dedekind.numbers.N;


public class TransposedVectorTest {

    @ParameterizedTest
    @MethodSource
    public void testTranspose(N b1) {

        final var v1 = one(b1);

        final Ket<N, ?, ?> v1t = transposed(v1);

        final Bra<N, ?, ?> v1tt = v1t.transpose();

        assertThat(v1tt).isEqualTo(v1);
    }

    private static Stream<Arguments> testTranspose() {
        return Stream.of(
                of(nat(0)),
                of(nat(1)),
                of(nat(123))
                );
    }

    @ParameterizedTest
    @MethodSource
    public void testOuterProduct(N b1, N b2, N b3, N expected) {

        // |1>
        final Ket<N, ?, ?> v1 = transposed(one(b1));

        // |1> <2|
        final var tensor = v1.outer(one(b2));

        // |1><2|3>
        final Ket<N, ?, ?> ket = tensor.apply(one(b3));

        // <3|2><1|
        final Bra<N, ?, ?> found = ket.transpose();

        // <4|
        final Bra<N, ?, ?> exp = one(expected);

        // <3|2><1| == <4|
        assertThat(found).isEqualTo(exp);
    }

    private static Stream<Arguments> testOuterProduct() {

        return Stream.of(
                of(nat(0), nat(1), nat(0), nat(0)),
                of(nat(1), nat(2), nat(3), nat(6))
                );
    }
}
