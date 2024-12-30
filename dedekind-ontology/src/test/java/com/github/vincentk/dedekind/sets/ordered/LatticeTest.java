package com.github.vincentk.dedekind.sets.ordered;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.N;
import com.github.vincentk.dedekind.sets.SingletonSet;

public class LatticeTest {

    private static final Lattice.Bounded<N.Nat, ?, ?> SUBJECT = new SingletonSet.Ordered.Default<>(N.ONE);

    @Test
    public void testRelations() {
	assertThat(N.ONE.upperBound(N.ONE)).isEqualTo(N.ONE);
	assertThat(N.ONE.upperBound(N.TWO)).isEqualTo(N.TWO);
	
	//assertThat(N.ONE.lowerBound(N.ONE)).isEqualTo(N.ONE);
	//assertThat(N.ONE.lowerBound(N.TWO)).isEqualTo(N.TWO);
    }

    @Test
    public void testDefaults() {
	assertThat(SUBJECT.isEmpty()).isFalse();
	assertThat(SUBJECT.top()).isEqualTo(N.ONE);
	assertThat(SUBJECT.bottom()).isEqualTo(N.ONE);
    }
}
