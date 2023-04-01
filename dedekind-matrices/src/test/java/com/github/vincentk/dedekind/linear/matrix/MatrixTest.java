package com.github.vincentk.dedekind.linear.matrix;

import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.bilinear.InnerProductSpace.Ket;
import com.github.vincentk.dedekind.numbers.R;

public class MatrixTest {

    @Test
    public void testMock() {

        @SuppressWarnings("unchecked")
        final Matrix<R, ?, ?, ?, ?, ?> subject = mock(Matrix.class);
        
        @SuppressWarnings("unused")
        final Ket<R, ?, ?> actual = subject.apply(null);
    }
}
