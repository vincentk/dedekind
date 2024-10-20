package com.github.vincentk.dedekind.sets;

import java.util.Optional;

/**
 * https://en.wikipedia.org/wiki/Tuple
 */
public interface Tuple<T>
extends
Set.Finite<Tuple<T>>
{
    public static class Empty<T1> implements Tuple<T1> {

	@Override
	public final Optional<Tuple<T1>> next() {
	    return Optional.empty();
	}

	@Override
	public long cardinality() {
	    return 0;
	}

    }

    public static <T1> Tuple<T1> empty() {
	return new Empty<>();
    }
}
