package com.github.vincentk.dedekind.binary.relation.homogeneous;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.B;
import com.github.vincentk.dedekind.sets.binary.relation.Pair;

public class IdentityTest {

    @Test
    public void testDefaultEquality() {

	final var pft = new Pair.Impl<>(B.FALSE, B.TRUE);
	assertTrue(pft.eq(pft));
	final var ptf = new Pair.Impl<>(B.TRUE, B.FALSE);
	assertTrue(ptf.eq(ptf));

	assertFalse(pft.eq(ptf));
	assertFalse(ptf.eq(pft));
    }
}
