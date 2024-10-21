package com.github.vincentk.dedekind.linear.primitives;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.numbers.R;
import com.github.vincentk.dedekind.relation.binary.homogeneous.Field;
import com.github.vincentk.dedekind.algebra.sets.Fields;

public class RsTest {

    @Test
    public void tsTest() {

        // Type check to trigger compilation error:
        Fields.Reals tc1 = R.ONE;

        Field.Reals<R> tc2 = R.ONE;

        assertThat(tc2.div(R.ONE)).isEqualTo(tc1);
    }
}
