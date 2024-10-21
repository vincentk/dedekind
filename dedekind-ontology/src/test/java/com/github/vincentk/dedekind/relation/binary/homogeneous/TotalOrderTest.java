package com.github.vincentk.dedekind.relation.binary.homogeneous;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.B;

public class TotalOrderTest {

    @Test
    public void testPartialOrder() {
	assertTrue(B.FALSE.lt(B.TRUE));
	assertTrue(!B.TRUE.lt(B.FALSE));
    }
}
