package com.github.vincentk.dedekind.linear;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.linear.finite.One;
import static com.github.vincentk.dedekind.linear.finite.One.of;
import com.github.vincentk.dedekind.linear.lazy.MatrixAddition;
import com.github.vincentk.dedekind.linear.lazy.MatrixMultiplication;
import com.github.vincentk.dedekind.numbers.Z;

public class OneByOneTest {

    @Test
    public void testAddition() {
        
        // x = {1}
        final One<Z> x = of(Z.ONE);
        
        // X = {x} = {{1}}
        final OneByOne<Z> X = new OneByOne<>(x);
        
        // X + X = X * 2
        final var X2 = X.plus(X);
        assertThat(X2).isInstanceOf(MatrixAddition.class);
        
        // 2 * X * {1} = {2}
        final var x2 = X2.apply(x);
        assertThat(x2).isEqualTo(of(Z.TWO));
        
        // {2} * {1} = 2
        assertThat(x2.dot(x)).isEqualTo(Z.TWO);
    }
    
    @Test
    public void testMultiplication() {
        
        // x = {1}
        final One<Z> x = of(Z.ONE);
        
        // X = {x} = {{1}}
        final OneByOne<Z> X = new OneByOne<>(x);
        
        // X + X = X * 2
        final var X2 = X.plus(X);
        assertThat(X2).isInstanceOf(MatrixAddition.class);
        
        // (2 * X) ^ 2 = {{4}}
        final var X4 = X2.compose(X2);
        assertThat(X4).isInstanceOf(MatrixMultiplication.class);
        
        // (2 * X) ^ 2 * {1} = {2}
        final var x4 = X4.apply(x);
        assertThat(x4).isEqualTo(of(Z.of(4)));
        
        // {2} * {1} = 2
        assertThat(x4.dot(x)).isEqualTo(Z.of(4));
    }
}
