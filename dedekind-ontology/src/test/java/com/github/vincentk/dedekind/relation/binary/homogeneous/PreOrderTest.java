package com.github.vincentk.dedekind.relation.binary.homogeneous;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.*;

import com.github.vincentk.dedekind.relation.binary.homogeneous.PreOrder.AntiSymmetric;

public class PreOrderTest {

    @SuppressWarnings("unchecked")
    @Test
    public void testAntiSymmetricPreOrderDefault() {
	final var subject = Mockito.mock(AntiSymmetric.class);
	
	when(subject.eq(subject)).thenReturn(true);
	
	assertThat(subject.leq(subject)).isEqualTo(false);
    }
}
