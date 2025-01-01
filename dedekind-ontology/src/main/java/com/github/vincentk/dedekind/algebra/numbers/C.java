package com.github.vincentk.dedekind.algebra.numbers;

import java.util.Optional;

import com.github.vincentk.dedekind.algebra.numbers.R.R64;
import com.github.vincentk.dedekind.algebra.structures.Field;
import com.github.vincentk.dedekind.algebra.structures.Vector;
import com.github.vincentk.dedekind.families.Pair;
import com.github.vincentk.dedekind.geometry.MetricSpace;
import com.github.vincentk.dedekind.sets.Cardinality;

/**
 * The set of complex numbers.
 */
public interface C
extends
Field.Complex<C.C64, Cardinality.Uncountable, C>,
MetricSpace<R.R64, Cardinality.Uncountable, C.C64, C>
{
    public static final C64 
    ZERO = complex(0, 0),
    R1 = complex(1, 0),
    I1 = complex(0, 1),
    UNIT = complex(1, 1);

    public static C64 complex(double re, double im) {
	return of(R.real(re), R.real(im));
    }

    public static C64 of(R64 re, R64 im) {
	return new Ce(re, im);
    }

    interface C64
    extends
    Field.Complex.Ce<C64>,
    MetricSpace.MeG<R64, C64>,
    Vector.Ve<R64, C64>,
    Pair.Homogeneous<R64, C64>
    {
	public R64 re();
	public R64 im();

	@Override
	default R64 fst() {
	    return re();
	}

	@Override
	default R64 snd() {
	    return im();
	}

	@Override
	default C64 plus(C64 that) {
	    return of(re().十(that.re()), im().十(that.im()));
	}

	@Override
	default C64 negate() {
	    return of(re().neg(), im().neg());
	}

	@Override
	default Optional<C64> inverse() {
	    final R64 r2 = re().x(re());
	    final Optional<R64> r2iO = r2.inv();
	    return r2iO.map(r2i -> conj().mult(r2i));
	}

	@Override
	default C64 mult(R64 scalar) {
	    return of(re().x(scalar), im().x(scalar));
	}

	@Override
	default C64 times(C64 that) {

	    final var r2 = re().x(that.re());
	    final var i2 = im().x(that.im());

	    final var ri = re().x(that.im());
	    final var ir = im().x(that.re());

	    return of(r2.minus(i2), ri.十(ir));
	}

	@Override
	default C64 conjugate() {
	    return of(re(), im().neg());
	}

	@Override
	default R64 abs() {
	    return times(conj()).re().sqrt();
	}
    }

    record Ce (R64 re, R64 im) implements C64 {

	@Override
	public boolean equals(Object that) {
	    if (!(that instanceof C64)) return false;
	    return this.eq((C64) that);
	}
    }
}
