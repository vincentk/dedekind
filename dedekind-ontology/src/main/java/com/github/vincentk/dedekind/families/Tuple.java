package com.github.vincentk.dedekind.families;

import java.util.List;

import com.github.vincentk.dedekind.algebra.numbers.B;
import com.github.vincentk.dedekind.algebra.numbers.N;
import com.github.vincentk.dedekind.algebra.sets.SemiRings;
import com.github.vincentk.dedekind.algebra.structures.SemiRing;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Set;
import com.github.vincentk.dedekind.sets.binary.relation.Pair;
import com.github.vincentk.dedekind.sets.ordered.Directed;

/**
 * Tuple ~ a finite sequence of number-like things.
 * 
 * In this case, we use {@link SemiRing} as a stand-in for being something like a number.
 * 
 * https://en.wikipedia.org/wiki/Tuple
 */
public interface Tuple<
T extends SemiRing<T>,
D extends Set.Finite<D> & Directed<Cardinality.Finite, D> & SemiRings.Naturals,
I extends Tuple<T, D, I>
>
extends
Sequence<T, Cardinality.Finite, D>
{
    int length();

    public interface Tuple2<
    T extends SemiRing<T>,
    D extends Set.Finite<D> & Directed<Cardinality.Finite, D> & SemiRings.Booleans
    >
    extends
    Tuple<T, D, Tuple2<T, D>>,
    Pair<T, T, Tuple2<T, D>>
    {
	default int length() {
	    return 2;
	}

	public record Two<T extends SemiRing<T>>
	(T fst, T snd)
	implements Tuple2<T, B> {

	    @Override
	    public T at(B d) {
		return d.eq(B.TRUE) ? fst : snd;
	    }
	}
    }
    
    public record TupleN<
    T extends SemiRing<T>,
    D extends N
    >
    (List<T> values)
    implements
    Tuple<T, N, TupleN<T, D>>
    {

	@Override
	public T at(N d) {
	    return values.get((int)d.integer());
	}

	@Override
	public int length() {
	    return values.size();
	}
    }
}