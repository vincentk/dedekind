package com.github.vincentk.dedekind.linear;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.linear.finite.One;
import static com.github.vincentk.dedekind.linear.finite.One.of;
import com.github.vincentk.dedekind.linear.lazy.MatrixAddition;
import com.github.vincentk.dedekind.linear.primitives.Zs;

public class OneByOneTest {

    @Test
    public void testAddition() {
        
        // x = {1}
        final One<Zs> x = of(Zs.ONE);
        
        // X = {x} = {{1}}
        final OneByOne<Zs> X = new OneByOne<>(x);
        
        // X + X = X * 2
        final var X2 = X.plus(X);
        assertThat(X2).isInstanceOf(MatrixAddition.class);
        
        // 2 * X * {1} = {2}
        final var x2 = X2.apply(x);
        assertThat(x2).isEqualTo(of(Zs.TWO));
        
        // {2} * {1} = 2
        assertThat(x2.dot(x)).isEqualTo(Zs.TWO);
    }
    
    @Test
    public void testMultiplication() {
        
        // x = {1}
        final One<Zs> x = of(Zs.ONE);
        
        // X = {x} = {{1}}
        final OneByOne<Zs> X = new OneByOne<>(x);
        
        // X + X = X * 2
        final var X2 = X.plus(X);
        assertThat(X2).isInstanceOf(MatrixAddition.class);
        
        // (2 * X) ^ 2 = {{4}}
        final var X4 = X2.compose(X2);
        
        // (2 * X) ^ 2 * {1} = {2}
        final var x4 = X4.apply(x);
        assertThat(x4).isEqualTo(of(Zs.of(4)));
        
        // {2} * {1} = 2
        assertThat(x4.dot(x)).isEqualTo(Zs.of(4));
    }
}
