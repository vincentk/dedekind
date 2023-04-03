/**
 * 
 */
package com.github.vincentk.dedekind.linear.vector;

import static com.github.vincentk.dedekind.linear.finite.One.one;
import static com.github.vincentk.dedekind.linear.vector.arrays.Booleans.booleans;
import static com.github.vincentk.dedekind.numbers.N.nat;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.params.provider.Arguments.of;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.github.vincentk.dedekind.algebra.binary.Bracket.Bra;
import com.github.vincentk.dedekind.linear.finite.One;
import com.github.vincentk.dedekind.linear.vector.arrays.Booleans;
import com.github.vincentk.dedekind.numbers.B;
import com.github.vincentk.dedekind.numbers.N;
import com.github.vincentk.dedekind.numbers.Z;
import com.github.vincentk.dedekind.sets.Cardinality;

public class ConcatenationTest {

    @ParameterizedTest
    @MethodSource
    public void testConcat1(N b1) {

        final One<Z> v1 = one(b1.asInt());

        final var tst = new Concatenation<Z, One<Z>, One<Z>, One<Z>, One<Z>>(v1, v1);

        final Bra<Z, ?, ?> sum = tst.plus(tst);

        assertThat(sum).isNotNull();

        final Z dt = tst.dot(tst.transpose());

        // [n, n]^2 = 2 * n^2
        final Z twoN2 = nat(2).asInt().times(b1.asInt().abs2());
        assertThat(dt).isEqualTo(twoN2);
    }

    private static Stream<Arguments> testConcat1() {
        return Stream.of(
                of(nat(0)),
                of(nat(1)),
                of(nat(123))
                );
    }

    @ParameterizedTest
    @MethodSource
    public void testConcat2(One<B> b1, Booleans<Cardinality.Finite> b2) {

        final var tst = new Concatenation<>(b1, b2);

        //assertEquals(tst.cardinality(), b1.cardinality() + b2.cardinality());

        // Can concatenate recursively:
        final var tst2 = new Concatenation<>(tst, tst);

        //assertEquals(tst2.cardinality(), 2 * tst.cardinality());
        
        // Can de-structure:
        assertThat(tst2.snd().fst()).isInstanceOf(One.class);
    }

    private static Stream<Arguments> testConcat2() {
        return Stream.of(
                of(one(B.bool(false)), booleans()),
                of(one(B.bool(false)), booleans(false)),
                of(one(B.bool(false)), booleans(false, true))
                );
    }
}
