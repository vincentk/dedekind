package com.github.vincentk.dedekind.sets.unary.function;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.N;

public class LambdaTest {

    private static final N.Ne
    ONE = N.ONE,
    TWO = N.TWO,
    FOUR = TWO.plus(TWO);

    @Test
    public void testAp() {
	assertThat(ONE.ap(ONE)).isEqualTo(TWO);	
    }

    @Test
    public void testAsFunction() {
	assertThat(ONE.asFunction().apply(ONE)).isEqualTo(TWO);	
    }

    @Test
    public void testAsRelation() {
	assertThat(ONE.asRelation().χ()).isEqualTo(TWO);
	assertThat(TWO.asRelation().χ()).isEqualTo(FOUR);
    }
}
