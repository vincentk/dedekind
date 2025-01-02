package com.github.vincentk.dedekind.sets;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.N;

public class UnionTest {

    private static final SingletonSet<N.N63.Ne, ?> S1 = new SingletonSet.Ordered.Default<>(N.ONE);
    private static final SingletonSet<N.N63.Ne, ?> S2 = new SingletonSet.Ordered.Default<>(N.TWO);
    private static final EmptySet<N.N63.Ne> EMPTY = EmptySet.empty();

    @Test
    public void testConstruction() {
	assertThat(EMPTY.union(EMPTY)).isEqualTo(EMPTY);
	assertThat(S1.union(EMPTY)).isEqualTo(S1);
	assertThat(EMPTY.union(S1)).isEqualTo(S1);
	assertThat(S1.union(S1)).isEqualTo(S1);

	final var u2 = S1.union(S2);
	assertThat(u2).isInstanceOf(Union.class);
	assertThat(u2.contains(N.ONE)).isTrue();
	assertThat(u2.contains(N.TWO)).isTrue();
    }
}
