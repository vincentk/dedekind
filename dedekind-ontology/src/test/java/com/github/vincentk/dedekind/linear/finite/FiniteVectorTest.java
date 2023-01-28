package com.github.vincentk.dedekind.linear.finite;

import static org.mockito.Mockito.mock;

import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

public class FiniteVectorTest {

    @Test
    public void testStreaming() {

        final FiniteVector<?, ?, ?> subject = mock(FiniteVector.class);

        @SuppressWarnings("unused")
        final Stream<?> result = subject.enumerate();
    }
}
