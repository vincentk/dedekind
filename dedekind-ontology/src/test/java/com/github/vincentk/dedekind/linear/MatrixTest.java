package com.github.vincentk.dedekind.linear;

import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.linear.primitives.Rs;

public class MatrixTest {

    @Test
    public void testMock() {

        @SuppressWarnings("unchecked")
        final Matrix<Rs, ?, ?, ?, ?, ?> subject = mock(Matrix.class);
        
        @SuppressWarnings("unused")
        final ColumnVector<Rs, ?, ?> actual = subject.apply(null);
    }
}
