/**
 * 
 */
package com.github.vincentk.dedekind.linear.finite;

import static com.github.vincentk.dedekind.linear.finite.Two.two;
import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.Z;

public class TwoTest {

    @Test
    public void twoTest() {

        // {0 1}
        final var t1 = two(Z.ZERO, Z.ONE);
        
        // {1 0}
        final var t2 = two(Z.ONE, Z.ZERO);
        
        // {0 1}' {1 0} = {{1 0} {0 0}}
        final var m1 = t1.transpose().outer(t2);
        
        // 1 {{1 0} {0 0}} = {{1 0} {0 0}}
        final var m2 = m1.mult(Z.ONE);
        
        // {{1 0} {0 0}} {1 1}' = {1 0}'
        final var t3 = m2.apply(two(Z.ONE, Z.ONE).transpose()).transpose();
        
        // {1 0} {1 1}' = {1}
        final var t4 = t3.dot(two(Z.ONE, Z.ONE).transpose());

        assertThat(t4).isEqualTo(Z.ONE);
    }
}
