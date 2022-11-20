package com.github.vincentk.dedekind.algebra;

import com.github.vincentk.dedekind.sets.Monoids;

public interface MonoidM<M extends MonoidM<M>> extends Monoids {
	
	M times(M that);
}