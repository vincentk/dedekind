package com.github.vincentk.dedekind.sets;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.B;

public class PoSetTest {

    @Test
    public void testPosetEq() {
	assertTrue(B.TRUE.eq(B.TRUE));
	assertTrue(B.FALSE.eq(B.FALSE));
	
	assertFalse(B.TRUE.eq(B.FALSE));
	assertFalse(B.FALSE.eq(B.TRUE));
    }
}
