/**
 * 
 */
package com.github.vincentk.dedekind.bilinear;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.Z;
import com.github.vincentk.dedekind.linear.finite.One;

public class OuterProductTest {

    private final One<Z> t1 = One.oneOf(Z.ZERO), f1 = One.oneOf(Z.ONE);

    private final OuterProduct<Z, ?, ?, ?, ?> subject = t1.transpose().outer(f1);    

    @Test
    public void testTranspose() {

        assertThat(subject.transpose().bra()).isEqualTo(t1);
    }

    @Test
    public void testMultiplication() {

        assertThat(subject.mult(Z.ZERO).ket()).isEqualTo(f1.transpose());
    }
}
