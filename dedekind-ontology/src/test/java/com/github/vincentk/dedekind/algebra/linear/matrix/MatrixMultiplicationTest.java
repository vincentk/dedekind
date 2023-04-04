/**
 * 
 */
package com.github.vincentk.dedekind.algebra.linear.matrix;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.linear.finite.One;
import com.github.vincentk.dedekind.numbers.B;

public class MatrixMultiplicationTest {
    
    @Test
    public void testMatrixMultiplication() {
        final var m1 = new OneByOne<>(One.one(B.bool(true)));
        final var m2 = new OneByOne<>(One.one(B.bool(false)));
        
        final var sut = m1.compose(m2);
        
        final Matrix<B, ?, ?, ?> res = sut.transpose();
        
        assertThat(res).isNotNull();
    }
}
