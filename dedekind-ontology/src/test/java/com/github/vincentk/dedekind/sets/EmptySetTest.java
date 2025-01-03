package com.github.vincentk.dedekind.sets;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.N;
import com.github.vincentk.dedekind.families.Sequence;

public class EmptySetTest {

    private static final EmptySet<N.N63.Ne> SUBJECT = EmptySet.empty();

    @Test
    public void testEmptySet() {

	assertTrue(SUBJECT.eq(SUBJECT));

	assertThat(SUBJECT.intersection(SUBJECT)).isEqualTo(SUBJECT);

	assertThat(SUBJECT.union(SUBJECT)).isEqualTo(SUBJECT);

	assertThat(SUBJECT.sub(SUBJECT))
	.as(() -> "The empty set is a subset of itself.")
	.isTrue();

	assertThat(SUBJECT.sup(SUBJECT))
	.as(() -> "The empty set is a super-set of itself.")
	.isTrue();

	assertThat(SUBJECT.where(x -> true))
	.as(() -> "A conditioned empty set is still empty.")
	.isEqualTo(SUBJECT);

	assertThat(SUBJECT.complement(SUBJECT))
	.as(() -> "The difference between the empty set and itself empty.")
	.isEqualTo(EmptySet.empty());
	
	assertThat(SUBJECT.enumerate())
	.as(() -> "The enumeration of the empty set is the empty sequence.")
	.isEqualTo(new Sequence.Finite.Empty<>());
    }
}
