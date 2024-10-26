package com.github.vincentk.dedekind.binary.relation.homogeneous;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import com.github.vincentk.dedekind.sets.binary.relation.homogeneous.PreOrder.AntiSymmetric;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.*;

public class PreOrderTest {

    @SuppressWarnings("unchecked")
    @Test
    public void testAntiSymmetricPreOrderDefault() {
	final var subject = Mockito.mock(AntiSymmetric.class);
	
	when(subject.eq(subject)).thenReturn(true);
	
	assertThat(subject.leq(subject)).isEqualTo(false);
    }
}
