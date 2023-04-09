/**
 * 
 */
package com.github.vincentk.dedekind.algebra.binary.linear;

import com.github.vincentk.dedekind.algebra.binary.SemiModule;
import com.github.vincentk.dedekind.algebra.binary.linear.MajorOrder.Cols;
import com.github.vincentk.dedekind.algebra.binary.linear.MajorOrder.Rows;
import com.github.vincentk.dedekind.algebra.unary.Monoid;
import com.github.vincentk.dedekind.algebra.unary.SemiRing;
import com.github.vincentk.dedekind.sets.AoC;
import com.github.vincentk.dedekind.sets.Cardinality;

interface Array<
F extends Monoid.P<F>,
O extends MajorOrder,
C extends Cardinality.Countable,
S extends Array<F, O, C, S>
>
extends
Monoid.P<S>,
AoC<F, AoC.Enumeration<F>>
{
    public abstract class OfSet<
    F extends SemiRing<F>,
    O extends MajorOrder,
    C extends Cardinality.Countable,
    V extends Array<F, O, C, V>
    >
    implements
    Array<F, O, C, V>
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
        public final Enumeration<F> enumeration() {
            return values;
        }
    }

    interface Vector<
    F extends SemiRing<F>,
    O extends MajorOrder,
    C extends Cardinality.Countable,
    S extends Vector<F, O, C, S>
    >
    extends
    Array<F, O, C, S>,
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

            public final class N<
            F extends SemiRing<F>,
            C extends Cardinality.Countable
            >
            extends Vector.K<F, Rows, C, N<F, C>>
            implements Row<F, C, N<F, C>>
            {
                public N(Enumeration<F> values) {
                    super(values);
                }

                @Override
                protected N<F, C> clone(Enumeration<F> values) {
                    return new N<>(values);
                }
            }
        }

        interface Col<
        F extends SemiRing<F>,
        C extends Cardinality.Countable,
        S extends Col<F, C, S>
        >
        extends Vector<F, Cols, C, S> {

            public final class Cm<
            F extends SemiRing<F>,
            C extends Cardinality.Countable
            >
            extends K<F, Cols, C, Cm<F, C>>
            implements Col<F, C, Cm<F, C>>
            {
                public Cm(Enumeration<F> values) {
                    super(values);
                }

                @Override
                protected Cm<F, C> clone(Enumeration<F> values) {
                    return new Cm<>(values);
                }
            }
        }
    }
}
