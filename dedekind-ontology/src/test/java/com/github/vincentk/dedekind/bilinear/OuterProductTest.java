/**
 * 
 */
package com.github.vincentk.dedekind.bilinear;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.Z;
import com.github.vincentk.dedekind.linear.finite.One;

public class OuterProductTest {

    private static final One<Z>
    // <0|
    ZERO = One.oneOf(Z.ZERO),
    // <1|
    ONE = One.oneOf(Z.ONE),
    // <2|
    TWO = One.oneOf(Z.TWO);

    // |0><1|
    private final OuterProduct<Z, ?, ?, ?, ?> subject = ZERO.transpose().outer(ONE);    

    @Test
    public void testTranspose() {

	// |1><0|
	final var t1 = subject.transpose();

	final var b1 = t1.bra();
	// <0| = <0|
	assertThat(b1).isEqualTo(ZERO);
    }

    @Test
    public void testMultiplication() {

	// |0><1| * 2 = |2 * 0><1| = |0><1|
	final var m1 = subject.mult(Z.TWO);

	// |0>
	final var k1 = m1.ket();
	assertThat(k1).isEqualTo(ZERO.transpose());
	

	// |1><1| * 2 = |2><1|
	final var m2 = ONE.transpose().outer(ONE).mult(Z.TWO);
	// |2>
	final var k2 = m2.ket();
	assertThat(k2).isEqualTo(TWO.transpose());
	
    }
}
