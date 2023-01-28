package com.github.vincentk.dedekind.linear.primitives;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.Field;
import com.github.vincentk.dedekind.sets.Fields;

public class RsTest {

    @Test
    public void tsTest() {

        // Type check to trigger compilation error:
        Fields.Reals tc1 = Rs.ONE;

        Field.Reals<Rs> tc2 = Rs.ONE;

        assertThat(tc2.div(Rs.ONE)).isEqualTo(tc1);
    }
}
