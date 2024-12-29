package com.github.vincentk.dedekind.linear.matrix;

import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.R;
import com.github.vincentk.dedekind.algebra.structures.Bracket.Ket;

public class MatrixTest {

    @Test
    public void testMock() {

        @SuppressWarnings("unchecked")
        final Matrix<R.R64, ?, ?, ?> subject = mock(Matrix.class);
        
        @SuppressWarnings("unused")
        final Ket<R.R64, ?, ?> actual = subject.apply(null);
    }
}
