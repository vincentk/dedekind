/**
 * 
 */
package com.github.vincentk.dedekind.algebra.binary.linear;

import static com.github.vincentk.dedekind.algebra.binary.linear.Array.zeroes;
import static org.assertj.core.api.Assertions.assertThat;

import java.util.Optional;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.binary.linear.Array.Vector.Col;
import com.github.vincentk.dedekind.numbers.N;
import com.github.vincentk.dedekind.sets.Cardinality.Countable;

public class ArrayTest {

    @Test
    public void testZeroes() {
        final Col<N, Countable, ?> zeroes = zeroes(N.ZERO);
        
        assertThat(zeroes.get(527)).isEqualTo(Optional.of(N.ZERO));
        
        assertThat(zeroes.range(0, 0)).isEmpty();
    }
}
