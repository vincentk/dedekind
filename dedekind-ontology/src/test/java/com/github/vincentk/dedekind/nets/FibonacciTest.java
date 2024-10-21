package com.github.vincentk.dedekind.nets;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.N;
import com.github.vincentk.dedekind.families.Sequence;
import com.github.vincentk.dedekind.families.Sequence.Fibonacci;

public class FibonacciTest {

    private static final Sequence.Fibonacci fib = new Fibonacci();
    
    /**
     * Tests that subset operation is available through sub-typing
     * by way of spelling out an explicit type check.
     */
    @Test
    public void testFibs() {
	assertThat(fib.at(N.ZERO)).isEqualTo(N.ZERO);
	assertThat(fib.at(N.ONE)).isEqualTo(N.ONE);
	assertThat(fib.at(N.TWO)).isEqualTo(N.ONE);
	assertThat(fib.at(N.nat(7))).isEqualTo(N.nat(13));
    }
}
