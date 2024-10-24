package com.github.vincentk.dedekind.binary.relation.homogeneous;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.B;

public class PartialOrderTest {

    @Test
    public void testPartialOrder() {
	assertTrue(B.FALSE.leq(B.TRUE));
	assertTrue(B.FALSE.lt(B.TRUE));

	assertTrue(!B.TRUE.leq(B.FALSE));
	assertTrue(!B.TRUE.lt(B.FALSE));
    }
}
