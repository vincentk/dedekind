package com.github.vincentk.dedekind.sets;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.N;

public class EmptySetTest {

    private static final EmptySet<N.Nat> SUBJECT = EmptySet.empty();

    @Test
    public void testPosetEq() {

	assertTrue(SUBJECT.eq(SUBJECT));

	assertThat(SUBJECT.intersection(SUBJECT)).isEqualTo(SUBJECT);

	assertThat(SUBJECT.union(SUBJECT)).isEqualTo(SUBJECT);

	assertThat(SUBJECT.sub(SUBJECT))
	.as(() -> "The empty set is a subset of itself.")
	.isEqualTo(true);

	assertThat(SUBJECT.sup(SUBJECT))
	.as(() -> "The empty set is a super-set of itself.")
	.isEqualTo(true);
	
	assertThat(SUBJECT.where(x -> true))
	.as(() -> "A conditioned empty set is still empty.")
	.isEqualTo(SUBJECT);

	/*
	assertTrue(B.TRUE.eq(B.TRUE));
	assertTrue(B.FALSE.eq(B.FALSE));

	assertFalse(B.TRUE.eq(B.FALSE));
	assertFalse(B.FALSE.eq(B.TRUE));
	 */
    }
}
