package com.github.vincentk.dedekind.families;

import java.util.List;
import java.util.stream.Collectors;

import com.github.vincentk.dedekind.relation.binary.Pair;
import com.github.vincentk.dedekind.relation.binary.SemiModule;
import com.github.vincentk.dedekind.relation.binary.homogeneous.SemiRing;
import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.algebra.numbers.B;
import com.github.vincentk.dedekind.algebra.numbers.N;
import com.github.vincentk.dedekind.algebra.numbers.Number;
import com.github.vincentk.dedekind.algebra.sets.SemiRings;
import com.github.vincentk.dedekind.sets.Set;

/**
 * Tuple ~ a finite sequence of number-like things.
 * 
 * In this case, we use {@link SemiRing} as a stand-in for being something like a number.
 * 
 * https://en.wikipedia.org/wiki/Tuple
 */
public interface Tuple<
T extends Number<T>,
D extends Set.Finite<D> & Set.Directed<Cardinality.Finite, D> & SemiRings.Naturals,
I extends Tuple<T, D, I>
>
extends
Sequence<T, Cardinality.Finite, D>,
SemiModule<T, I>
{
    int length();

    public interface Tuple2<
    T extends Number<T>,
    D extends Set.Finite<D>  & Set.Directed<Cardinality.Finite, D> & SemiRings.Booleans
    >
    extends
    Tuple<T, D, Tuple2<T, D>>,
    Pair<T, T>
    {
	default int length() {
	    return 2;
	}

	public record Two<T extends Number<T>>
	(T fst, T snd)
	implements Tuple2<T, B> {

	    @Override
	    public T at(B d) {
		return d.equals(B.TRUE) ? fst : snd;
	    }

	    @Override
	    public Tuple2<T, B> mult(T scalar) {
		return new Two<>(
			fst.times(scalar),
			snd.times(scalar));
	    }

	    @Override
	    public Tuple2<T, B> plus(Tuple2<T, B> module) {
		return new Two<>(
			fst.times(module.fst()),
			snd.times(module.snd()));
	    }
	}
    }
    
    public record TupleN<
    T extends Number<T>,
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
	public TupleN<T, D> mult(T scalar) {
	    final var l2 = values.stream().map(x -> x.times(scalar)).collect(Collectors.toList());
	    return new TupleN<>(l2);
	}

	@Override
	public TupleN<T, D> plus(TupleN<T, D> module) {
	    // TODO Auto-generated method stub
	    return null;
	}


	@Override
	public int length() {
	    return values.size();
	}

    }
}