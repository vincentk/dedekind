package com.github.vincentk.dedekind.relation.binary.homogeneous;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.B;

public class TotalOrderTest {

    @Test
    public void testPartialOrder() {
	assertTrue(B.FALSE.lt(B.TRUE));
	assertTrue(!B.TRUE.lt(B.FALSE));
	
	assertThat(B.FALSE.upperBound(B.TRUE)).isEqualTo(B.TRUE);
    }
    
    @Test
    public void testSemiLatticeJoin() {
	assertThat(B.FALSE.join(B.FALSE)).isEqualTo(B.FALSE);
	assertThat(B.FALSE.join(B.TRUE)).isEqualTo(B.TRUE);
	assertThat(B.TRUE.join(B.FALSE)).isEqualTo(B.TRUE);
	assertThat(B.TRUE.join(B.TRUE)).isEqualTo(B.TRUE);
    }
    
    @Test
    public void testSemiLatticeMeet() {
	assertThat(B.FALSE.meet(B.FALSE)).isEqualTo(B.FALSE);
	assertThat(B.FALSE.meet(B.TRUE)).isEqualTo(B.FALSE);
	assertThat(B.TRUE.meet(B.FALSE)).isEqualTo(B.FALSE);
	assertThat(B.TRUE.meet(B.TRUE)).isEqualTo(B.TRUE);
    }
}
