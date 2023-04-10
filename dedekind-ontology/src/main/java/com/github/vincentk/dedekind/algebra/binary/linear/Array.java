/**
 * 
 */
package com.github.vincentk.dedekind.algebra.binary.linear;

import java.util.Optional;

import com.github.vincentk.dedekind.algebra.binary.SemiModule;
import com.github.vincentk.dedekind.algebra.binary.linear.Array.Vector.Col.Cm;
import com.github.vincentk.dedekind.algebra.binary.linear.MajorOrder.Cols;
import com.github.vincentk.dedekind.algebra.binary.linear.MajorOrder.Rows;
import com.github.vincentk.dedekind.algebra.peano.Peano.Succ;
import com.github.vincentk.dedekind.algebra.peano.Peano.Zero;
import com.github.vincentk.dedekind.algebra.unary.Monoid;
import com.github.vincentk.dedekind.algebra.unary.SemiRing;
import com.github.vincentk.dedekind.sets.AoC;
import com.github.vincentk.dedekind.sets.Cardinality;

interface Array<
F extends Monoid.P<F>,
O extends MajorOrder,
C extends Cardinality.Countable,
S extends Monoid.P<S>
>
extends
Monoid.P<S>,
AoC<F, AoC.Enumeration<F>>
{
    /**
     * @param min
     * @return equivalent to [min, ...)
     */
    Optional<? extends Array<F, O, C, ?>> skip(long min);
    
    /**
     * @param max
     * @return equivalent to [0, max)
     */
    Optional<? extends Array<F, O, Cardinality.Finite, ?>> limit(long max);
    
    /**
     * @param min
     * @param max
     * @return [min, max)
     */
    default Optional<? extends Array<F, O, Cardinality.Finite, ?>> range(long min, long max) {
        
        if (min >= max) {
            return Optional.empty();
        }
        
        return limit(max).flatMap(lmt -> lmt.skip(min));
    }
    
    /**
     * Semi-ring over semi-rings through pairwise addition and multiplication of elements.
     * 
     * @param <F>
     * @param <O>
     * @param <C>
     * @param <V>
     */
    public abstract class OfSet<
    F extends SemiRing<F>,
    O extends MajorOrder,
    C extends Cardinality.Countable,
    V extends Array<F, O, C, V> & SemiRing<V>
    >
    implements
    Array<F, O, C, V>,
    SemiRing<V>
    {
        private final Enumeration<F> values;

        public OfSet(Enumeration<F> values) {
            this.values = values;
        }

        protected abstract V clone(Enumeration<F> values);

        @Override
        public final V zero() {
            return clone(enumeration().map(x -> x.zero()));
        }

        @Override
        public final V plus(V that) {
            final var pairs = enumeration().zip(that.enumeration());
            final var sums = pairs.map(p -> p.a().plus(p.b()));
            return clone(sums);
        }
        
        @Override
        public final V times(V that) {
            final var pairs = enumeration().zip(that.enumeration());
            final var sums = pairs.map(p -> p.a().times(p.b()));
            return clone(sums);
        }

        @Override
        public final Enumeration<F> enumeration() {
            return values;
        }
        
        @Override
        public Optional<V> skip(long min) {
            return Optional.of(clone(enumeration().skip(min)));
        }
    }

    interface Vector<
    F extends SemiRing<F>,
    O extends MajorOrder,
    C extends Cardinality.Countable,
    S extends SemiModule<F, C, S> & SemiRing<S>
    >
    extends
    Array<F, O, C, S>,
    // In order to support vectors of vectors, the type
    // system really wants this to be a semi-ring
    // (i.e. support a notion of pairwise multiplication).
    // This can be satisfied via e.g. pairwise multiplication
    // of elements:
    SemiRing<S>,
    SemiModule<F, C, S>
    {

        public abstract class K<
        F extends SemiRing<F>,
        O extends MajorOrder,
        C extends Cardinality.Countable,
        V extends Vector.K<F, O, C, V>
        >
        extends OfSet<F, O, C, V>
        implements Vector<F, O, C, V>
        {
            public K(Enumeration<F> values) {
                super(values);
            }

            @Override
            public final V mult(F scalar) {
                return clone(enumeration().map(x -> x.times(scalar)));
            }
        }

        interface Row<
        F extends SemiRing<F>,
        C extends Cardinality.Countable,
        S extends Row<F, C, S>
        >
        extends
        Vector<F, Rows, C, S>,
        InnerProduct<F, C, Col<F, C, ?>, S>
        {
            @Override
            default F dot(Col<F, C, ?> that) {
                final var pairs = enumeration().zip(that.enumeration());
                final var products = pairs.map(p -> p.a().times(p.b()));
                final var sum = products.fold((x, y) -> x.plus(y));
                return sum.get();
            }

            public class N<
            F extends SemiRing<F>,
            C extends Cardinality.Countable
            >
            extends Vector.K<F, Rows, C, N<F, C>>
            implements
            Row<F, C, N<F, C>>,
            LinearMap<F, C, Cm<F,C>, Succ<Zero>, Cm<F,Succ<Zero>>, N<F, C>>
            {
                public N(Enumeration<F> values) {
                    super(values);
                }

                @Override
                protected N<F, C> clone(Enumeration<F> values) {
                    return new N<>(values);
                }

                @Override
                public Optional<N<F, Cardinality.Finite>> limit(long max) {
                    return enumeration().limit(max).map(x -> new N<>(x));
                }

                @Override
                public Cm<F, Succ<Zero>> apply(Cm<F, C> that) {
                    final F scalar = this.dot(that);
                    
                    final Enumeration<F> once = Enumeration.repeat(scalar).limit(1).get();
                    
                    return new Cm<>(once);
                }
            }
        }

        interface Col<
        F extends SemiRing<F>,
        C extends Cardinality.Countable,
        S extends Col<F, C, S>
        >
        extends Vector<F, Cols, C, S> {

            public class Cm<
            F extends SemiRing<F>,
            C extends Cardinality.Countable
            >
            extends K<F, Cols, C, Cm<F, C>>
            implements
            Col<F, C, Cm<F, C>>,
            SemiModule<F, C, Cm<F, C>>
            {
                public Cm(Enumeration<F> values) {
                    super(values);
                }

                @Override
                protected Cm<F, C> clone(Enumeration<F> values) {
                    return new Cm<>(values);
                }

                @Override
                public Optional<Cm<F, Cardinality.Finite>> limit(long max) {
                    return enumeration().limit(max).map(x -> new Cm<>(x));
                }
            }
        }
    }
    
    static <F extends SemiRing<F>>
    Vector.Col<F, Cardinality.Countable, ?> zeroes(F f0) {
        
        final var z0 = Optional.of(f0.zero());
        
        return new Vector.Col.Cm<>(() -> z0);
    }
}
