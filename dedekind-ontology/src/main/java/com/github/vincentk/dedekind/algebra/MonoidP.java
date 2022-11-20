package com.github.vincentk.dedekind.algebra;

import com.github.vincentk.dedekind.sets.Monoids;

public interface MonoidP<M extends MonoidP<M>> extends Monoids {
	
	M plus(M that);
	
	// Poor man's operator overloading:
	default M p(M that) {
		return plus(that);
	}
}