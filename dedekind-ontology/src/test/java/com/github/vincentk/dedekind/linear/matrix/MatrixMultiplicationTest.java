/**
 * 
 */
package com.github.vincentk.dedekind.linear.matrix;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.Z;
import com.github.vincentk.dedekind.linear.finite.One;

public class MatrixMultiplicationTest {
    
    @Test
    public void testMatrixMultiplication() {
        final var m1 = new OneByOne<>(One.oneOf(Z.ONE));
        final var m2 = new OneByOne<>(One.oneOf(Z.ZERO));
        
        final var sut = m1.compose(m2);
        
        final Matrix<?, ?, ?, ?> res = sut.transpose();
        
        assertThat(res).isNotNull();
    }
}
