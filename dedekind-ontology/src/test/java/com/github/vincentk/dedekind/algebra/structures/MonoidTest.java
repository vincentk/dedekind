package com.github.vincentk.dedekind.algebra.structures;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.B;
import com.github.vincentk.dedekind.algebra.numbers.N;

public class MonoidTest {

    @Test
    public void testIdentitytP() {

	assertTrue(N.ZERO.isIdentityP());
	assertFalse(N.ONE.isIdentityP());

	assertTrue(B.FALSE.isIdentityP());
	assertFalse(B.TRUE.isIdentityP());
    }
}
