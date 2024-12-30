package com.github.vincentk.dedekind.sets;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.github.vincentk.dedekind.algebra.numbers.N;

public class SingletonSetTest {

    private static final SingletonSet.Ordered<N.Nat, ?> SUBJECT = new SingletonSet.Ordered.Default<>(N.ONE);
    private static final EmptySet<N.Nat> EMPTY = EmptySet.empty();

    @Test
    public void testSingletonSet() {

	assertTrue(SUBJECT.eq(SUBJECT));
	
	assertThat(SUBJECT.intersection(SUBJECT)).isEqualTo(SUBJECT);
	assertThat(SUBJECT.intersection(EMPTY)).isEqualTo(EMPTY);
	
	assertThat(SUBJECT.union(SUBJECT)).isEqualTo(SUBJECT);
	assertThat(SUBJECT.union(EMPTY)).isEqualTo(SUBJECT);
	
	assertThat(SUBJECT.sub(SUBJECT))
	.as(() -> "The singleton set is a subset of itself.")
	.isTrue();
	assertThat(SUBJECT.sub(EMPTY))
	.as(() -> "The singleton set is not a subset of the empty set.")
	.isFalse();

	assertThat(SUBJECT.sup(SUBJECT))
	.as(() -> "The singleton set is a super-set of itself.")
	.isTrue();
	assertThat(SUBJECT.sup(EMPTY))
	.as(() -> "The singleton set is a super-set of the empty set.")
	.isTrue();

	assertThat(SUBJECT.where(x -> true))
	.as(() -> "The set of all elements of a singleton set is the singleton set.")
	.isEqualTo(SUBJECT);
	assertThat(SUBJECT.where(x -> false))
	.as(() -> "The set of all elements of a singleton set is the singleton set.")
	.isEqualTo(EMPTY);

	assertThat(SUBJECT.complement(SUBJECT))
	.as(() -> "The difference between the singleton set and itself is empty.")
	.isEqualTo(EMPTY);
	assertThat(SUBJECT.complement(EMPTY))
	.as(() -> "The difference between the singleton set the empty set is the singleton set.")
	.isEqualTo(SUBJECT);
    }
}
