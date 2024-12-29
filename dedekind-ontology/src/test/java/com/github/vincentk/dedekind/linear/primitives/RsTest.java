package com.github.vincentk.dedekind.linear.primitives;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Optional;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.R;
import com.github.vincentk.dedekind.algebra.numbers.R.R64;

public class RsTest {

    @Test
    public void tsTest() {

        // Type check to trigger compilation error:
        R64 tc1 = R.ONE;

        R64 tc2 = R.ONE;

        assertThat(tc2.divide(R.ONE)).isEqualTo(Optional.of(tc1));
    }
}
