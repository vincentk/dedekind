/**
 * 
 */
package com.github.vincentk.dedekind.sets;

import java.util.Optional;
import java.util.function.BinaryOperator;
import java.util.function.Function;

/**
 * The Axiom of Choice so to speak.
 * 
 * I.e. we can enumerate the elements.
 * 
 * @see https://en.wikipedia.org/wiki/Axiom_of_choice
 * @see https://en.wikipedia.org/wiki/Sequence
 */
@FunctionalInterface
public interface AoC<T, E extends AoC.Enumeration<T>> {
    E enumeration();

    @FunctionalInterface
    public interface Enumeration<T> {

        Optional<T> next();

        default Enumeration<T> skip(long n) {
            for (long i = 0; i < n; i++) {
                if (next().isEmpty()) break;
            }
            return this;
        }

        default <S> Enumeration<S> map(Function<T, S> f) {
            return () -> next().map(f);
        }

        record Pair<A, B>(A a, B b) {}

        default <S> Enumeration<Pair<T, S>> zip(Enumeration<S> that) {
            return () -> next().flatMap(x -> that.next().map(y -> new Pair<>(x, y)));
        }

        default T fold(T t0, BinaryOperator<T> op) {

            return next()
                    // recurse:
                    .map(t1 -> fold(op.apply(t1, t0), op))
                    // base case:
                    .orElse(t0);
        }
        
        default Optional<T> fold(BinaryOperator<T> op) {
            return next().map(fst -> fold(fst, op));
        }
    }
}
