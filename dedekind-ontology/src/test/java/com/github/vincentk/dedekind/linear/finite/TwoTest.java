/**
 * 
 */
package com.github.vincentk.dedekind.linear.finite;

import static com.github.vincentk.dedekind.linear.finite.Two.two;
import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.B;

public class TwoTest {

    @Test
    public void twoTest() {

        // {0 1}
        final var t1 = two(B.FALSE, B.TRUE);
        
        // {1 0}
        final var t2 = two(B.TRUE, B.FALSE);
        
        // {0 1}' {1 0} = {{1 0} {0 0}}
        final var m1 = t1.transpose().outer(t2);
        
        // 1 {{1 0} {0 0}} = {{1 0} {0 0}}
        final var m2 = m1.mult(B.TRUE);
        
        // {{1 0} {0 0}} {1 1}' = {1 0}'
        final var t3 = m2.apply(two(B.TRUE, B.TRUE).transpose()).transpose();
        
        // {1 0} {1 1}' = {1}
        final var t4 = t3.dot(two(B.TRUE, B.TRUE).transpose());

        assertThat(t4).isEqualTo(B.TRUE);
    }
}
