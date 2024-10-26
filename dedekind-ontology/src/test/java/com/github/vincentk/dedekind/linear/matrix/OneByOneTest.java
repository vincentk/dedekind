package com.github.vincentk.dedekind.linear.matrix;

import static com.github.vincentk.dedekind.linear.finite.One.oneOf;
import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.Z;
import com.github.vincentk.dedekind.linear.finite.One;

public class OneByOneTest {

    @Test
    public void testAddition() {

        // x = {1}
        final One<Z> x = oneOf(Z.ONE);

        // X = {x} = {{1}}
        final OneByOne<Z> X = new OneByOne<>(x);

        // X + X = X * 2
        final var X2 = X.plus(X);
        assertThat(X2).isInstanceOf(MatrixAddition.class);

        // 2 * X * {1} = {2}
        final var x2 = X2.apply(x.transpose()).transpose();
        assertThat(x2).isEqualTo(oneOf(Z.TWO));

        // {2} * {1} = 2
        assertThat(x2.dot(x.transpose())).isEqualTo(Z.TWO);
    }

    @Test
    public void testMultiplication() {

        // x = {1}
        final One<Z> x = oneOf(Z.ONE);

        // X = {x} = {{1}}
        final OneByOne<Z> X = new OneByOne<>(x);

        // X + X = X * 2
        final var X2 = X.plus(X);
        assertThat(X2).isInstanceOf(MatrixAddition.class);

        // (2 * X) ^ 2 = {{4}}
        final var X4 = X2.compose(X2);
        assertThat(X4).isInstanceOf(MatrixMultiplication.class);

        // (2 * X) ^ 2 * {1} = {2}
        final var x4 = X4.apply(x.transpose()).transpose();
        assertThat(x4).isEqualTo(oneOf(Z.integer(4)));

        // {2} * {1} = 2
        assertThat(x4.dot(x.transpose())).isEqualTo(Z.integer(4));
    }

    @Test
    public void testComposition() {
        // x = {2}
        final One<Z> x = oneOf(Z.TWO);

        // X = {x} = {{2}}
        final OneByOne<Z> X = new OneByOne<>(x);

        // {{2}} {{2}} {3} = {{ 4 }} {3} = {12}
        final var result = X.compose(X).apply(oneOf(Z.integer(3)).transpose()).transpose();

        assertThat(result).isEqualTo(oneOf(Z.integer(12)));
    }
}
