/**
 * 
 */
package com.github.vincentk.dedekind.bilinear;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.linear.finite.One;
import com.github.vincentk.dedekind.numbers.B;

public class OuterProductTest {

    private final One<B> t1 = One.one(B.TRUE), f1 = One.one(B.FALSE);

    private final OuterProduct<B, ?, ?, ?, ?> subject = t1.transpose().outer(f1);    

    @Test
    public void testTranspose() {

        assertThat(subject.transpose().bra()).isEqualTo(t1);
    }

    @Test
    public void testMultiplication() {

        assertThat(subject.mult(B.FALSE).ket()).isEqualTo(f1.transpose());
    }
}
