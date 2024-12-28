package com.github.vincentk.dedekind.binary.relation;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.N;

public class RelationTest {
    
    private static final N.Ne
    ONE = N.ONE,
    TWO = N.TWO,
    FOUR = TWO.plus(TWO);
    
    @Test
    public void testAsRelation() {
	assertTrue(ONE.asRelation().χ(TWO));
	assertTrue(TWO.asRelation().χ(FOUR));
    }
}
