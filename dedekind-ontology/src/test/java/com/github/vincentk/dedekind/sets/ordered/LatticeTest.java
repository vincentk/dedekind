package com.github.vincentk.dedekind.sets.ordered;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

public class LatticeTest {

    private static final Lattice<?, ?, ?> SUBJECT = Mockito.mock(Lattice.class);
    
    @Test
    public void testDefaults() {
	assertThat(SUBJECT.isEmpty()).isFalse();
    }
}
