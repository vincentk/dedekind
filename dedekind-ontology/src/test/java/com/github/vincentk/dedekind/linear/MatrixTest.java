package com.github.vincentk.dedekind.linear;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.linear.Matrix.RowMajor;
import com.github.vincentk.dedekind.linear.primitives.Rs;

import static  org.mockito.Mockito.*;

public class MatrixTest {
    
    private static final Rs ZERO = Rs.ZERO;

    @Test
    public void testMock() {

        @SuppressWarnings("unchecked")
        final RowMajor<Rs, ?, ?, ?, ?> subject = mock(RowMajor.class);
        
        final Rs actual = subject.apply((RowVector<?, ?, ?>) null);
    }
}
