package com.github.vincentk.dedekind.families;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.C;

public class PairTest {

    @Test
    public void testEq() {
	
	assertTrue(C.R1.eq(C.R1));
	assertTrue(C.I1.eq(C.I1));
	assertFalse(C.R1.eq(C.I1));
    }
}
