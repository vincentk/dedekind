/**
 * 
 */
package com.github.vincentk.dedekind.linear.vector.arrays;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.params.provider.Arguments.of;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.github.vincentk.dedekind.numbers.B;
import com.github.vincentk.dedekind.sets.Cardinality;

public class BooleansTest {

    @ParameterizedTest
    @MethodSource
    public void testSelf(boolean[] b1) {
        
        final Booleans<Cardinality.Finite> sut = new Booleans<>(b1);
        
        assertEquals(sut.cardinality(), b1.length);
        
        assertThat(sut.dot(sut.transpose())).isNotNull();
        
        final Booleans<Cardinality.Finite> zeros = sut.mult(B.of(false));
        
        assertThat(zeros.dot(zeros.transpose())).isEqualTo(B.of(false));
        
        assertThat(sut.plus(sut)).isNotNull();
    }

    private static Stream<Arguments> testSelf() {
        return Stream.of(
                of(new boolean[] {}),
                of(new boolean[] {false}),
                of(new boolean[] {true}),
                of(new boolean[] {false, true})
                );
    }
}
