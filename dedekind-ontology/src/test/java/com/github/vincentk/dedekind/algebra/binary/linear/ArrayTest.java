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
        final Col<N, Countable, ?> z0 = zeroes(N.ZERO);
        
        assertThat(z0).as("Array construction.").isNotNull();

        assertThat(z0.get(527)).as("Check array indexing of infinite array.").isEqualTo(Optional.of(N.ZERO));

        assertThat(z0.limit(0)).as("Limit with negative-semidefinite value.").isEmpty();

        assertThat(z0.limit(1)).as("Limit with positive value.").isPresent();

        assertThat(z0.range(-1, 0)).as("Empty range.").isEmpty();

        assertThat(z0.range(1, 1)).as("Empty range (max >= min).").isEmpty();

        assertThat(z0.range(1, 2)).as("Non-empty range.").isPresent();
    }
}
