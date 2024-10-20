package com.github.vincentk.dedekind.nets;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.nets.Sequence.Fibonacci;
import com.github.vincentk.dedekind.numbers.N;

public class FibonacciTest {

    private static final Sequence.Fibonacci fib = new Fibonacci();
    
    /**
     * Tests that subset operation is available through sub-typing
     * by way of spelling out an explicit type check.
     */
    @Test
    public void testFibs() {
	assertThat(fib.apply(N.ZERO)).isEqualTo(N.ZERO);
	assertThat(fib.apply(N.ONE)).isEqualTo(N.ONE);
	assertThat(fib.apply(N.TWO)).isEqualTo(N.ONE);
	assertThat(fib.apply(N.nat(7))).isEqualTo(N.nat(13));
    }
}
