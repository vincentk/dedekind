/**
 * 
 */
package com.github.vincentk.dedekind.algebra.binary;

import static com.github.vincentk.dedekind.algebra.numbers.N.nat;
import static com.github.vincentk.dedekind.linear.finite.One.one;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.params.provider.Arguments.of;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.github.vincentk.dedekind.algebra.numbers.N;
import com.github.vincentk.dedekind.algebra.numbers.Z;
import com.github.vincentk.dedekind.sets.relation.binary.Transposed;
import com.github.vincentk.dedekind.sets.relation.binary.Bracket.Bra;
import com.github.vincentk.dedekind.sets.relation.binary.Bracket.Ket;


public class TransposedTest {

    @ParameterizedTest
    @MethodSource
    public void testTranspose(N b1) {

        final var v1 = one(b1.asInt());

        final Ket<Z, ?, ?> v1t = new Transposed<>(v1);

        final Bra<Z, ?, ?> v1tt = v1t.transpose();

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
        final Ket<Z, ?, ?> v1 = one(b1.asInt()).transpose();

        // |1> <2|
        final var tensor = v1.outer(one(b2.asInt()));

        // |1><2|3>
        final Ket<Z, ?, ?> ket = tensor.apply(one(b3.asInt()).transpose());

        // <3|2><1|
        final Bra<Z, ?, ?> found = ket.transpose();

        // <4|
        final Bra<Z, ?, ?> exp = one(expected.asInt());

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
